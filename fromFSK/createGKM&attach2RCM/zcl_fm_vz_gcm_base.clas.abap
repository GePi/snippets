class ZCL_FM_VZ_GCM_BASE definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_FM_CM_CONSTANTS .
  interfaces ZIF_FM_VZ_GCM .

  constants MC_BUKRS type BUKRS value '1000' ##NO_TEXT.
  constants CC_PRCTR_100 type PRCTR value '0000000100' ##NO_TEXT.

  methods GET_GCM_RECORD_ID
    returning
      value(RV_RECID) type ZGCM_RM_RECORD-RECORD_ID .
  class-methods DISPLAY_GCMCONTENT
    importing
      !IV_RECID type ZGCM_RM_RECORD_ID_ED
    exceptions
      EX_FAILED .
  methods GET_ATTRIBUT
    importing
      !IV_ATR_NAME type SRMADID
    returning
      value(RV_ATR_VAL) type STRING
    exceptions
      EX_FAILED .
protected section.

  data MO_CASE type ref to CL_SCMG_CASE .
  data MS_BO type BORIDENT .
  data MS_RECORD type ZGCM_RM_RECORD .
  data MT_ATTRVAL type ZGCM_RM_ATTR_VAL_T .
  data MO_CASE_WIN type ref to CL_SCMG_CASE_VISUALIZATION_WIN .

  methods ATTACH_GCM .
  methods ATTACH
    exceptions
      EX_FAILED .
  methods FILLDATA
    exceptions
      EX_FAILED .
  methods SEND
    exceptions
      EX_FAILED .
  methods SETATTRIBUTES
  abstract
    exceptions
      EX_FAILED .
  methods BUILDPDF
  abstract
    exporting
      !EV_FILENAME type FILE_NAME
      !EV_FILEPATH type ZGCM_RM_FILE_PATH_ED
      !EV_FILEFORMAT type ZGCM_RM_FILE_FORMAT_ED
      !EV_FILESIZE type ZGCM_RM_FILE_SIZE_ED
      !ET_CONTENTS type SRM_T_BINTAB
    exceptions
      EX_FAILED .
  methods GETATTRIBUTES
    returning
      value(RT_ATTR) type TY_NAMEVALUEASSTRING .
private section.

  data MT_CONTENTS type SRM_T_BINTAB .

  methods CREATE
    exceptions
      EX_FAILED .
  methods OPENCASE
    importing
      value(IV_CASEGUID) type SCMG_CASE_GUID optional
      value(IV_EXTKEY) type SCMG_EXT_KEY optional
    exceptions
      EX_FAILED .
ENDCLASS.



CLASS ZCL_FM_VZ_GCM_BASE IMPLEMENTATION.


  METHOD attach.
*======================================================================*
* Наименование   : Прикрепление контента
* Описание       : Прикрепление контента
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
      lf_filesize TYPE zgcm_rm_file_size_ed,
      lf_response TYPE string,
      lf_value    TYPE string.

    DATA:
      lt_contents TYPE srm_t_bintab,
      lt_log      TYPE zcmt_errors_t.


*-- формируем формуляр
    me->buildpdf(
      IMPORTING
        ev_filesize   = lf_filesize
        et_contents   = lt_contents
      EXCEPTIONS
        ex_failed        = 1
        OTHERS        = 2
    ).
    IF sy-subrc NE 0.
      MESSAGE e006 RAISING ex_failed.
    ENDIF.

*-- помещаем карточку в хранилище
    me->send(
      EXCEPTIONS
        ex_failed = 1
        OTHERS = 2
    ).
    IF sy-subrc NE 0.
      RAISE ex_failed.
    ENDIF.

*-- вкладываем сформированный файл в карточку GCM
    CALL FUNCTION 'Z_CM_GCM2_SET_CONTENT'
      EXPORTING
        iv_record_id      = me->ms_record-record_id
        iv_commit         = if_srm=>false
        it_content_bin    = lt_contents
        iv_file_name      = 'ЗНП.pdf'(001)
        iv_file_extension = 'pdf'
        iv_content_size   = lf_filesize
      IMPORTING
        et_log            = lt_log.

    LOOP AT lt_log TRANSPORTING NO FIELDS WHERE error_type CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      MESSAGE e005 RAISING ex_failed.
    ENDIF.

    TRY .
        lf_value = me->ms_record-record_id.
        me->mo_case->set_single_attribute_value(
          EXPORTING
            im_value   = lf_value
            im_srmadid = zif_fm_cm_constants=>cs_attr-recid
        ).
      CATCH cx_root.
        MESSAGE e003 RAISING ex_failed.
    ENDTRY.
  ENDMETHOD.


  METHOD attach_gcm.
*======================================================================*
* Наименование   : Присвоение GCM карточки к карточке ЗНП
* Описание       : Присвоение GCM карточки к карточке ЗНП
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
      lv_value TYPE string.

    MOVE me->ms_record-record_id TO lv_value.

    TRY.
        me->mo_case->set_single_attribute_value(
          EXPORTING
            im_srmadid = zif_fm_cm_constants=>cs_attr-recid
            im_value   = lv_value
        ).
      CATCH cx_srm_framework.    " Framework (abstract)
        RETURN.
      CATCH cx_scmg_case_attribute.    " Case Attributes
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD create.
*======================================================================*
* Наименование   : Создание карточки
* Описание       : Создание карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
      ls_log TYPE bal_s_msg.

    DATA:
      lt_log TYPE bal_t_msg.
    DATA:
      ls_bo TYPE borident,
      lt_bo TYPE tt_bor_identification.

    CALL FUNCTION 'Z_CA_GCM_API_CREATE_LOCAL'
      EXPORTING
        is_record     = me->ms_record
        is_object     = me->ms_bo
      IMPORTING
        et_log        = lt_log
        es_record     = me->ms_record
      EXCEPTIONS
        error_message = 1
        OTHERS        = 99.


    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        RAISING ex_failed.
    ENDIF.

    IF lt_log IS NOT INITIAL.
      READ TABLE lt_log INDEX 1 INTO ls_log.
      MESSAGE ID ls_log-msgid TYPE ls_log-msgty NUMBER ls_log-msgno
        WITH ls_log-msgv1 ls_log-msgv2 ls_log-msgv3 ls_log-msgv4
        RAISING ex_failed.
    ENDIF.
  ENDMETHOD.


  METHOD display_gcmcontent.
*======================================================================*
* Наименование   : Просмотр содержимого (скан-копии) GCM карточки
* Описание       : Просмотр содержимого (скан-копии) GCM карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
      lf_file_bin TYPE xstring,
      lf_tmstmp   TYPE timestampl.

    DATA:
      ls_log      TYPE bal_s_msg.

    DATA:
      lt_log      TYPE bal_t_msg.

    CHECK iv_recid IS NOT INITIAL.

    CALL FUNCTION 'Z_CA_GCM_OPEN_DOCUMENT_CONTENT'
      EXPORTING
        iv_record_id = iv_recid.

  ENDMETHOD.


  METHOD filldata.
*======================================================================*
* Наименование   : Подготовка данных для формирования карточки
* Описание       : Подготовка данных для формирования карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*

*-- заполняем данные БО
    me->ms_bo-objtype = zif_fm_cm_constants=>cs_bo-case.
    me->ms_bo-objkey  = me->mo_case->get_guid( ).

*-- заполняем основные аттрибуты карточки

    me->ms_record-created_by = get_attribut( if_scmg_case_read=>created_by ).
    me->ms_record-created_at = get_attribut( if_scmg_case_read=>create_time ).
    me->ms_record-changed_by = get_attribut( if_scmg_case_read=>changed_by ).
    me->ms_record-changed_at = get_attribut( if_scmg_case_read=>change_time ).

    "вместо БЕ - МВП!

    get_attribut(
      EXPORTING
        iv_atr_name ='ZZVZ_PRCTR_PARTN'
      RECEIVING rv_atr_val = me->ms_record-prctr
      EXCEPTIONS ex_failed = 4
    ).

    IF sy-subrc <> 0 AND me->ms_record-prctr IS INITIAL.
      me->ms_record-prctr = cc_prctr_100.
    ENDIF.
    me->ms_record-bukrs = me->ms_record-prctr+6(4).

*-- заполняем дополнительные аттрибуты
    me->setattributes(
      EXCEPTIONS
        ex_failed = 1
        OTHERS = 2
    ).
    IF sy-subrc NE 0.
      RAISE ex_failed.
    ENDIF.
  ENDMETHOD.


  METHOD getattributes.
*======================================================================*
* Наименование   : Получение аттрибутов
* Описание       : Получение аттрибутов
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
      l_value_objects TYPE srm_list_attribute_value,
      wa_value        LIKE LINE OF rt_attr,
      l_fieldname     TYPE fieldname,
      l_authority     LIKE sy-subrc,
      l_reselect_db   TYPE srmboolean.
    FIELD-SYMBOLS: <wa_value_object> LIKE LINE OF l_value_objects.
    TRY.

        l_value_objects = me->mo_case->get_attributes( ).
      CATCH cx_root.
        RETURN.
    ENDTRY.
* get value for each attribute:
    LOOP AT l_value_objects ASSIGNING <wa_value_object>.
      TRY.
          wa_value-name = <wa_value_object>->get_id( ).

          wa_value-value = me->mo_case->get_single_attribute_value( wa_value-name ).
        CATCH cx_root .
          RETURN.
      ENDTRY.

      APPEND wa_value TO rt_attr.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_attribut.
*======================================================================*
* Наименование   : Получение аттрибутов
* Описание       : Получение аттрибутов
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    TRY.
        rv_atr_val = me->mo_case->get_single_attribute_value( iv_atr_name ).
      CATCH cx_root.
        MESSAGE e003 RAISING ex_failed.
    ENDTRY.

  ENDMETHOD.


  METHOD get_gcm_record_id.
*======================================================================*
* Наименование   : Получить ИД созданной карточки ГКМ
* Описание       : Получить ИД созданной карточки ГКМ
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    CLEAR rv_recid.
    rv_recid = me->ms_record-record_id.

  ENDMETHOD.


  METHOD opencase.
*======================================================================*
* Наименование   : Открыть карточку
* Описание       : Открыть карточку
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
      lv_caseguid TYPE scmg_case_guid.

    DATA:
      lo_case_api TYPE REF TO if_scmg_case_api.

    IF iv_caseguid IS INITIAL AND iv_extkey IS INITIAL.
      MESSAGE e001 RAISING ex_failed.
    ENDIF.

    IF iv_caseguid IS INITIAL.
      lv_caseguid = zcl_fm_cm_services=>get_caseguid_by_extkey( iv_extkey ).
    ELSE.
      lv_caseguid = iv_caseguid.
    ENDIF.

    cl_scmg_case_api=>open_case(
      EXPORTING
        im_case_guid          = lv_caseguid
        im_enqueue            = if_srm=>true
        im_mode               = if_srm_sp_enqueue=>mode_exclusive
        im_update_task        = if_srm=>true
      RECEIVING
        re_case               = lo_case_api
      EXCEPTIONS
        failed                = 1
        enqueue_failed        = 2
        invalid_guid          = 3
        cx_srm_gsp_back       = 4
        no_authority          = 5
        illegal_case_type     = 6
        OTHERS                = 7
    ).

    CASE sy-subrc.
      WHEN 0.
        me->mo_case ?= lo_case_api->get_backend_case( ).
      WHEN OTHERS.
        CASE sy-subrc.
          WHEN 1.
*-- Ошибка открытия карточки на обработку
            MESSAGE e033 RAISING ex_failed.
          WHEN 2.
*-- Блокировка карточки для обработки не удалась
            MESSAGE e034 RAISING ex_failed.
          WHEN 3.
*-- Задан неверный GUID карточки
            MESSAGE e035 RAISING ex_failed.
          WHEN 5.
*-- Отсутствуют необходимые полномочия для обработки карточки
            MESSAGE e036 RAISING ex_failed.
          WHEN 6.
*-- Неверный вид карточки
            MESSAGE e037 RAISING ex_failed.
          WHEN OTHERS.
*-- Неизвестная ошибка при открытии карточки для обработки
            MESSAGE e038 RAISING ex_failed.
        ENDCASE.
    ENDCASE.
*--} End
  ENDMETHOD.


  METHOD send.
*======================================================================*
* Наименование   : Отправка карточки в хранилище
* Описание       : Отправка карточки в хранилище
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 20.07.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
      ls_log TYPE bal_s_msg,
      ls_bo  TYPE borident.

    DATA:
      lt_log TYPE bal_t_msg.

*-- помещаем карточку GCM в хранилище
    CALL FUNCTION 'Z_CA_GCM_API_SEND_TO_STORAGE'
      EXPORTING
        iv_commit   = if_srm=>false
        is_record   = me->ms_record
        it_attr_val = me->mt_attrval
        is_object   = me->ms_bo
      IMPORTING
        et_log      = lt_log
        es_record   = me->ms_record.
    LOOP AT lt_log TRANSPORTING NO FIELDS WHERE msgty CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      MESSAGE e007 RAISING ex_failed.
    ENDIF.

    IF me->ms_record-record_id IS INITIAL.
      MESSAGE e008 RAISING ex_failed.
    ENDIF.

*-- обновляем статус
    CLEAR me->ms_record-status.
    CALL FUNCTION 'Z_CA_GCM_MODIFY_RECORD_UPD'
      EXPORTING
        is_record = me->ms_record
        is_object = me->ms_bo.
  ENDMETHOD.


  METHOD zif_fm_vz_gcm~build.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Маркова И.В.
* Дата создания  : 31.03.2019
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
    lo_backend TYPE REF TO cl_scmg_case.

    IF ii_case IS INITIAL.
*-- открытие карточки
      me->opencase(
        EXPORTING
          iv_caseguid = iv_caseguid
          iv_extkey   = iv_extkey
        EXCEPTIONS
          ex_failed      = 1
          OTHERS      = 2
      ).
      IF sy-subrc NE 0.
        RAISE ex_failed.
      ENDIF.
    ELSE.
      me->mo_case ?= ii_case.
    ENDIF.

*-- подготовка данных для формирования карточки
    me->filldata(
      EXCEPTIONS
        ex_failed = 1
        OTHERS = 2
    ).
    IF sy-subrc NE 0.
      RAISE ex_failed.
    ENDIF.

*-- формурование карточки GCM (без сохранения)
    me->create(
      EXCEPTIONS
        ex_failed = 1
        OTHERS = 2
    ).
    IF sy-subrc NE 0.
      RAISE ex_failed.
    ENDIF.

    me->attach(
      EXCEPTIONS
        ex_failed = 1
        OTHERS = 2
    ).
    IF sy-subrc NE 0.
      RAISE ex_failed.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
