class ZCL_FM_VZ_GCM_CONTRACT definition
  public
  inheriting from ZCL_FM_VZ_GCM_BASE
  final
  create public .

public section.

  constants CV_GCM_DOCTYPE_MS01 type ZGCM_RM_RECORD-DOC_TYPE value 'ddt_001_003_003_099' ##NO_TEXT.
  constants CV_GCM_DOCTYPE_MS02 type ZGCM_RM_RECORD-DOC_TYPE value 'ddt_001_003_003_098' ##NO_TEXT.
  constants CV_CASE_CATEGORY_MS01 type SCMG_T_CASE_ATTR-CATEGORY value 'MS01' ##NO_TEXT.
  constants CV_CASE_CATEGORY_MS02 type SCMG_T_CASE_ATTR-CATEGORY value 'MS02' ##NO_TEXT.
  data CV_ANCHOR_GCM_AGR type STRING value 'GCM_AGR' ##NO_TEXT.
  constants CV_BOR_TYPE_ZCA_GCMREC type OJ_NAME value 'ZCA_GCMREC' ##NO_TEXT.
  constants CV_SPS_ID_GCMREC_AGR type STRING value 'ZCM_D_GCMREC_AGR' ##NO_TEXT.
  constants CV_RMS_ID_ZCNTR_AREA type STRING value 'ZCNTR_AREA' ##NO_TEXT.

  methods CREATE
    importing
      !IO_ACT_VZ_FORM type ref to ZCL_FM_VZ_FORM_ACT_VZ
      !IO_CASE type ref to CL_SCMG_CASE_VISUALIZATION_WIN .
  methods CREATE_BOR_LINK
    importing
      !IV_RECID type ZGCM_RM_RECORD-RECORD_ID
    returning
      value(RV_FL_CREATED) type FLAG .
  methods DEL_GCM_AND_CLEARING_BOR_LINK
    returning
      value(RV_FL_SUCCES) type FLAG .
  methods REFRESH_CASE_INTERFACE
    importing
      !IO_RECORD_POID type ref to IF_SRM_POID optional .
  methods TEST_API
    importing
      !IV_RECID type ZGCM_RM_RECORD-RECORD_ID .
  methods CREATE_LINK_BOR2GCM
    importing
      !IV_RECID type ZGCM_RM_RECORD-RECORD_ID
    returning
      value(RV_FL_CREATED) type FLAG .
protected section.

  data MO_ACT_VZ_FORM type ref to ZCL_FM_VZ_FORM_ACT_VZ .

  methods BUILDPDF
    redefinition .
  methods FILLDATA
    redefinition .
  methods SETATTRIBUTES
    redefinition .
  methods ATTACH
    redefinition .
private section.

  aliases BUILD
    for ZIF_FM_VZ_GCM~BUILD .
ENDCLASS.



CLASS ZCL_FM_VZ_GCM_CONTRACT IMPLEMENTATION.


  METHOD attach.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
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
        iv_file_name      = 'Печатная форма взаимозачета.pdf'(001)
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
  ENDMETHOD.


  METHOD buildpdf.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    IF mo_act_vz_form->print_data1 IS NOT INITIAL.
      mo_act_vz_form->get_pdf_print1(
       IMPORTING es_output = DATA(ls_output) ).
    ELSEIF mo_act_vz_form->print_data2 IS NOT INITIAL.
      mo_act_vz_form->get_pdf_print2(
        IMPORTING es_output = ls_output ).
    ELSEIF mo_act_vz_form->print_data3 IS NOT INITIAL.
      mo_act_vz_form->get_pdf_print3(
        IMPORTING es_output = ls_output ).
    ELSEIF mo_act_vz_form->print_data4 IS NOT INITIAL.
      mo_act_vz_form->get_pdf_print4(
        IMPORTING es_output = ls_output ).
    ENDIF.

    ev_fileformat = 'PDF'(005).
    ev_filename = 'Печатная форма взаимозачета'(002) && '.' && ev_fileformat .

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = ls_output-pdf
      IMPORTING
        output_length = ev_filesize
      TABLES
        binary_tab    = et_contents.

  ENDMETHOD.


  METHOD create.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    mo_case_win = io_case.
    mo_case ?= io_case->g_backend.
    mo_act_vz_form = io_act_vz_form.

    DATA(lv_category) = get_attribut( 'CATEGORY' ).
    IF lv_category <> cv_case_category_ms01 AND
       lv_category <> cv_case_category_ms02.
      RETURN.
    ENDIF.

    " Сохранить все изменения в текущей карточке
    TRY.
        io_case->save_case( im_no_dialog = 'X' ).
      CATCH cx_scmg_attr_display
            cx_srm_initialization
            cx_srm_sp_client
            cx_srm_framework
            cx_scmg.
        RETURN.
    ENDTRY.

    " Удалить существующую привязку и ВСЕ карточки ГКМ
    " привязанные к данному CASE с типами:
    " - CV_GCM_DOCTYPE_MS01
    " - CV_GCM_DOCTYPE_MS02
    IF NOT del_gcm_and_clearing_bor_link( ).
      RETURN.
    ENDIF.

    " Создать ГКМ карточку и получить её ID
    build( EXPORTING ii_case  = mo_case
           EXCEPTIONS OTHERS  = 4 ).
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      " Ошибка создания GCM
      MESSAGE s044(zfm_rkz0011_mess) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lv_recid) = get_gcm_record_id( ).
    COMMIT WORK AND WAIT.
    IF sy-subrc <> 0 OR lv_recid IS INITIAL.
      "Ошибка получения номера GCM
      refresh_case_interface( ).
      MESSAGE s045(zfm_rkz0011_mess) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Создание связи между допником (CASE) и созданной ГКМ
    IF NOT create_link_bor2gcm( lv_recid ).
      RETURN.
    ENDIF.

    " Обновление визуального интерфейса
    refresh_case_interface( ).
    "Карточка GCM &1 создана
    MESSAGE s047(zfm_vz_vzzachet) WITH lv_recid .

  ENDMETHOD.


  METHOD create_bor_link.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
*    " объекты API
*    DATA:
*      lo_case           TYPE REF TO cl_scmg_case,
*      lo_srm            TYPE REF TO if_srm,
*      lo_client_service TYPE REF TO if_srm_srm_client_service,
*      lo_poid           TYPE REF TO if_srm_poid,
*      lo_element        TYPE REF TO if_srm_sp_record_element.
*    " объект-коннектор и вспомогательные структуры
*    DATA:
*      lo_connector TYPE REF TO cl_srm_rmf_connector,
*      lv_descr     TYPE string,
*      ls_poid      TYPE srmpoid,
*      lt_poid      TYPE srm_list_poid,
*      lv_recid     TYPE zgcm_rm_record-record_id.
*
*    TRY. " Сохранение необходимо для передачи в API актуальных данных
*        " Но нельзя сохранять если были ошибки!!!
*        mo_case->if_scmg_case~store(
*          EXPORTING
*            im_do_validation = 'X'    " Boolean: Possible Values IF_SRM=>TRUE / FALSE
*        ).
*      CATCH cx_srm_framework.    " Framework (abstract)
*      CATCH cx_scmg.    " General Exception of Case Management
*      CATCH cx_srm_sp_client.    " Exception from SP Client
*    ENDTRY.
*
*    lo_case = mo_case.
*    lv_recid = iv_recid.
*
*    " Инициализация API
*    TRY .
*        DATA(lo_record_poid) = lo_case->if_scmg_case_read~get_record_poid( ).
*
*        lo_srm ?= lo_record_poid.
*        DATA(lo_root) = lo_srm->get_root( ).
*        DATA(lo_execution) = lo_root->get_sps_execution_by_poid( lo_record_poid ).
*
*        DATA lo_rec_be           TYPE REF TO if_srm_sp_record.
*        lo_rec_be ?= lo_execution->srm_connection( lo_record_poid ).
*        DATA(lt_element_tab) = lo_rec_be->element_get_all( ).
*
*
*        DATA lo_expert TYPE REF TO if_srm_sp_record_expert.
*        lo_expert ?= lo_rec_be.
*        lo_expert->set_lock_mode( cl_scmg_case_visualization_win=>if_scmg_sp_case_client~c_mode_modify ).
*        lo_rec_be->open( for_update = if_srm=>true ).
*
*      CATCH cx_srm_initialization
*            cx_srm_registration_data cx_srm_poid cx_srm_connection
*            cx_srm_sp_record cx_srm_gsp_back.
*        rv_fl_created = abap_false.
*        RETURN.
*    ENDTRY.
*
*    " Создание объекта-связи
*    TRY .
*        CREATE OBJECT lo_connector
*          EXPORTING
*            sps_id = cv_sps_id_gcmrec_agr.
*      CATCH cx_srm_initialization
*            cx_srm_registration_data.
*        rv_fl_created = abap_false.
*        RETURN.
*    ENDTRY.
*
*    lo_srm = lo_connector->get_if_srm( ).
*    lo_client_service ?= lo_srm->get_srm_service( ).
*
*    " Формирование таблицы со значениям POID
*    ls_poid-id    = srmmr_dscr_sppoid_bor_id.
*    ls_poid-value = lv_recid.
*    INSERT ls_poid INTO TABLE lt_poid.
*
*    ls_poid-id    = srmmr_dscr_sppoid_bor_type.
*    ls_poid-value = cv_bor_type_zca_gcmrec.
*    INSERT ls_poid INTO TABLE lt_poid.
*
*    TRY .
*        lo_poid = lo_client_service->poid_get_instance(
*            im_sps_id  = cv_sps_id_gcmrec_agr
*            im_rms_id  = cv_rms_id_zcntr_area
*            im_sp_poid = lt_poid ).
*
*        " Создание элемент для GCM карточки
*        lo_element = lo_rec_be->element_create( ).
*
*        lv_descr = lv_recid .
*        SHIFT lv_descr LEFT DELETING LEADING '0'.
*
*        lv_descr ='Карточка ГКМ'(003) && ' ('  && lv_descr && `)`.
*        lo_element->description_set( lv_descr ).
*
*        lo_element->type_set( if_srm_sp_record_element=>type_instance ).
*        lo_element->if_srm_sp_record_elem_instance~poid_set( lo_poid ).
*
*      CATCH cx_srm_framework
*            cx_srm_sp_record.
*        rv_fl_created = abap_false.
*        RETURN.
*    ENDTRY.
*
*
*    " Присоединение объекта-связи через API
*    TRY.
*        lo_rec_be->element_add_by_anchor(
*          EXPORTING
*            anchor           = cv_anchor_gcm_agr
*            element          = lo_element
*            stacked          = abap_false
*        ).
*      CATCH cx_srm.
*        rv_fl_created = abap_false.
*        RETURN.
*    ENDTRY.
*
*    " Сохранение привязки
*    TRY .
*        lo_rec_be->save( new_version = if_srm=>false ).
*      CATCH cx_srm_sp_record.
*        rv_fl_created = abap_false.
*        RETURN.
*      CATCH cx_srm_gsp_back.
*        rv_fl_created = abap_false.
*        RETURN.
*    ENDTRY.
*
*    " Закрытие случая
*    TRY .
*        lo_rec_be->close( ).
*      CATCH cx_srm_sp_record.
*        rv_fl_created = abap_false.
*        RETURN.
*    ENDTRY.
*
*    " Обновление компонент соед. объекты
*    " Заодно это избавит от ошибки про XML-render
*    refresh_case_interface( lo_record_poid ).
*
*    rv_fl_created = abap_true.
  ENDMETHOD.


  METHOD create_link_bor2gcm.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA: lv_func_name TYPE rs38l_fnam VALUE 'Z_CA_GCM_DELETE_DOCUMENT'.

    TYPES BEGIN OF lty_gcm_bukrs.
    TYPES record_id TYPE zgcm_rm_record-record_id.
    TYPES doc_type  TYPE zgcm_rm_record-doc_type.
    TYPES document_id TYPE zgcm_rm_record-document_id.
    TYPES END OF lty_gcm_bukrs.

    TYPES BEGIN OF lty_bor_records.
    INCLUDE TYPE scmg_bor_in_record.
    TYPES record_id TYPE zgcm_rm_record-record_id.
    TYPES document_id TYPE zgcm_rm_record-document_id.
    TYPES END OF lty_bor_records.

    DATA lt_gcm_bukrs   TYPE TABLE OF lty_gcm_bukrs.
    DATA ls_gcm_bukrs   LIKE LINE  OF lt_gcm_bukrs.
    DATA lo_case_api    TYPE REF TO if_scmg_case_api.
    DATA lv_bor_id      TYPE string.
    DATA lt_bor_objects TYPE scmg_tt_bor_in_record.
    DATA lt_bor_records TYPE TABLE OF lty_bor_records.
    DATA: lr_record_id TYPE RANGE OF zgcm_rm_record-record_id,
          ls_record_id LIKE LINE OF lr_record_id,
          li_rec_api   TYPE REF TO if_srm_sp_record.
    DATA lv_guid TYPE scmg_case_guid.
    DATA lv_description TYPE string.

    rv_fl_created = abap_false.

    zcl_fm_scmg_services=>open_case(
        EXPORTING
          im_enqueue            = abap_true
          im_case_guid          = mo_case->get_guid( )
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

    IF sy-subrc <> 0 OR lo_case_api IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    CALL METHOD lo_case_api->bor_objects_get
      EXPORTING
        im_anchor      = cv_anchor_gcm_agr
        im_bor_type    = cv_bor_type_zca_gcmrec
      RECEIVING
        re_bor_objects = lt_bor_objects
      EXCEPTIONS
        failed         = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      zcl_fm_scmg_services=>close_case( lo_case_api ).
      RETURN.
    ENDIF.

    " Карточка нужного типа уже привязана
    IF lt_bor_objects IS NOT INITIAL.
      rv_fl_created = abap_true.
      zcl_fm_scmg_services=>close_case( lo_case_api ).
      RETURN.
    ENDIF.

    IF get_attribut( 'CATEGORY' ) = cv_case_category_ms01.
      lv_description = 'Соглашение о взаимозачёте'(006) && ` ` &&
                       |{ iv_recid ALPHA = OUT }|.
    ELSE.
      lv_description = 'Заявление о взаимозачёте'(008) && ` ` &&
                       |{ iv_recid ALPHA = OUT }|.
    ENDIF.


    lo_case_api->bor_object_insert(
      EXPORTING
        im_anchor   = cv_anchor_gcm_agr
        im_bor_key  = CONV #( iv_recid )
        im_bor_type = cv_bor_type_zca_gcmrec
        im_description = lv_description
        im_sps_id = cv_sps_id_gcmrec_agr
      EXCEPTIONS
        failed     = 1
        OTHERS     = 2 ).
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 DISPLAY LIKE sy-msgty.
      zcl_fm_scmg_services=>close_case( lo_case_api ).
      RETURN.
    ENDIF.

    CALL METHOD lo_case_api->save
      EXPORTING
        im_dequeue = abap_true
      EXCEPTIONS
        failed     = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 DISPLAY LIKE sy-msgty.
      zcl_fm_scmg_services=>close_case( lo_case_api ).
      RETURN.
    ENDIF.

    rv_fl_created = abap_true.

    zcl_fm_scmg_services=>close_case( lo_case_api ).
  ENDMETHOD.


  METHOD del_gcm_and_clearing_bor_link.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA: lv_func_name TYPE rs38l_fnam VALUE 'Z_CA_GCM_DELETE_DOCUMENT'.

    TYPES BEGIN OF lty_gcm_bukrs.
    TYPES record_id TYPE zgcm_rm_record-record_id.
    TYPES doc_type  TYPE zgcm_rm_record-doc_type.
    TYPES document_id TYPE zgcm_rm_record-document_id.
    TYPES END OF lty_gcm_bukrs.

    TYPES BEGIN OF lty_bor_records.
    INCLUDE TYPE scmg_bor_in_record.
    TYPES record_id TYPE zgcm_rm_record-record_id.
    TYPES document_id TYPE zgcm_rm_record-document_id.
    TYPES END OF lty_bor_records.

    DATA lt_gcm_bukrs   TYPE TABLE OF lty_gcm_bukrs.
    DATA ls_gcm_bukrs   LIKE LINE  OF lt_gcm_bukrs.
    DATA lo_case_api    TYPE REF TO if_scmg_case_api.
    DATA lv_bor_id      TYPE string.
    DATA lt_bor_objects TYPE scmg_tt_bor_in_record.
    DATA lt_bor_records TYPE TABLE OF lty_bor_records.
    DATA: lr_record_id TYPE RANGE OF zgcm_rm_record-record_id,
          ls_record_id LIKE LINE OF lr_record_id,
          li_rec_api   TYPE REF TO if_srm_sp_record.
    DATA lv_guid TYPE scmg_case_guid.

    FIELD-SYMBOLS <fs_bor_objects> LIKE LINE OF lt_bor_objects.
    FIELD-SYMBOLS <fs_bor_records> LIKE LINE OF lt_bor_records.


*    TRY. " Сохранение необходимо для передачи в API актуальных данных
*        " Но нельзя сохранять если были ошибки!!!
*        mo_case->if_scmg_case~store(
*          EXPORTING
*            im_do_validation = 'X'    " Boolean: Possible Values IF_SRM=>TRUE / FALSE
*        ).
*      CATCH cx_srm_framework.    " Framework (abstract)
*      CATCH cx_scmg.    " General Exception of Case Management
*      CATCH cx_srm_sp_client.    " Exception from SP Client
*    ENDTRY.

    zcl_fm_scmg_services=>open_case(
        EXPORTING
          im_enqueue            = abap_true
          im_case_guid          = mo_case->get_guid( )
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

    IF sy-subrc <> 0 OR lo_case_api IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 DISPLAY LIKE sy-msgty.
      rv_fl_succes = abap_false.
      RETURN.
    ENDIF.

    CALL METHOD lo_case_api->bor_objects_get
      EXPORTING
        im_anchor      = cv_anchor_gcm_agr
        im_bor_type    = cv_bor_type_zca_gcmrec
      RECEIVING
        re_bor_objects = lt_bor_objects
      EXCEPTIONS
        failed         = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      zcl_fm_scmg_services=>close_case( lo_case_api ).
      rv_fl_succes = abap_false.
      RETURN.
    ENDIF.

    IF lt_bor_objects IS INITIAL.
      rv_fl_succes = abap_true.
      RETURN.
    ENDIF.

    LOOP AT lt_bor_objects ASSIGNING <fs_bor_objects>.
      APPEND INITIAL LINE TO lt_bor_records ASSIGNING <fs_bor_records>.
      MOVE-CORRESPONDING <fs_bor_objects> TO <fs_bor_records>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_bor_records>-bor_key
        IMPORTING
          output = <fs_bor_records>-record_id.
    ENDLOOP.

    SELECT record_id
           bukrs
           doc_type
           document_id
      FROM zgcm_rm_record
      INTO CORRESPONDING FIELDS OF TABLE lt_gcm_bukrs
      FOR ALL ENTRIES IN lt_bor_records
      WHERE record_id EQ lt_bor_records-record_id.
    IF sy-subrc <> 0.
      CLEAR lt_bor_records.
    ENDIF.

    LOOP AT lt_bor_records ASSIGNING <fs_bor_records>.

      CLEAR ls_gcm_bukrs.
      READ TABLE lt_gcm_bukrs INTO ls_gcm_bukrs
        WITH KEY record_id = <fs_bor_records>-record_id.

      " Удалить карточки GCM ЭЦП
      IF  ls_gcm_bukrs-doc_type EQ cv_gcm_doctype_ms01 OR
          ls_gcm_bukrs-doc_type EQ cv_gcm_doctype_ms02.
        CALL FUNCTION lv_func_name
          EXPORTING
            iv_record_id = <fs_bor_records>-record_id.

        " Удалить связи в CASE
        lv_bor_id = <fs_bor_records>-id.
        CALL METHOD lo_case_api->bor_object_delete
          EXPORTING
            im_id              = lv_bor_id
          EXCEPTIONS
            failed             = 1
            invalid_parameters = 2
            OTHERS             = 3.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CALL METHOD lo_case_api->save
      EXPORTING
        im_dequeue = abap_true
      EXCEPTIONS
        failed     = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      zcl_fm_scmg_services=>close_case( lo_case_api ).
      ROLLBACK WORK.
      rv_fl_succes = abap_false.
      RETURN.
    ENDIF.

    zcl_fm_scmg_services=>close_case( lo_case_api ).


    " Проверим, остались ли GCM для карочки. возможно, что-то не удалилось
    lv_guid = mo_case->get_guid( ).
    SELECT rec~record_id
           rec~doc_type
      FROM zgcm_rm_bo_link AS link
      JOIN zgcm_rm_record AS rec
        ON rec~record_id = link~record_id
      INTO CORRESPONDING FIELDS OF TABLE lt_gcm_bukrs
     WHERE link~objtype EQ zif_fm_vz_constants=>mc_objtyp_scase
       AND link~objkey  = lv_guid
       AND ( ( rec~doc_type EQ cv_gcm_doctype_ms01 ) OR
             ( rec~doc_type EQ cv_gcm_doctype_ms02 ) ).
    IF sy-subrc = 0.
      " Удаление из таблиц
      LOOP AT lt_gcm_bukrs INTO ls_gcm_bukrs.
        CALL FUNCTION lv_func_name
          EXPORTING
            iv_record_id = ls_gcm_bukrs-record_id.
      ENDLOOP.
    ENDIF.

    COMMIT WORK AND WAIT.

    IF sy-subrc = 0.
      rv_fl_succes = abap_true.
    ELSE.
      rv_fl_succes = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD filldata.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
*-- заполняем основные аттрибуты карточки
    me->ms_bo-objtype = zif_fm_cm_constants=>cs_bo-case.
    me->ms_bo-objkey  = me->mo_case->get_guid( ).

    me->ms_record-created_by = get_attribut( if_scmg_case_read=>created_by ).
    me->ms_record-created_at = get_attribut( if_scmg_case_read=>create_time ).
    me->ms_record-changed_by = get_attribut( if_scmg_case_read=>changed_by ).
    me->ms_record-changed_at = get_attribut( if_scmg_case_read=>change_time ).

    get_attribut( EXPORTING iv_atr_name ='PRCTR'
                  RECEIVING rv_atr_val = me->ms_record-prctr
                  EXCEPTIONS ex_failed = 4 ).
    IF sy-subrc <> 0 AND me->ms_record-prctr IS INITIAL.
      me->ms_record-prctr = zcl_fm_vz_gcm_base=>cc_prctr_100.
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


  METHOD refresh_case_interface.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
      lo_subcomponent_be     TYPE REF TO if_scmg_subcomponent_backend,
      lo_subcomponent_record TYPE REF TO cl_scmg_subcomponent_record,
      lo_case                TYPE REF TO cl_scmg_case,
      lo_record_poid         TYPE REF TO if_srm_poid.

    lo_case ?= mo_case_win->g_backend.

    IF io_record_poid IS SUPPLIED.
      lo_record_poid = io_record_poid.
    ELSE.
      lo_record_poid = lo_case->if_scmg_case_read~get_record_poid( ).
    ENDIF.

    LOOP AT mo_case_win->g_tab_subcomponents
      ASSIGNING FIELD-SYMBOL(<lfs_subcomponent>).
      TRY .
          lo_subcomponent_be ?= <lfs_subcomponent>-class.
          CHECK lo_subcomponent_be IS BOUND.

          IF lo_subcomponent_be->get_type( ) = if_scmg_subcomponent_backend=>type_sub_record.
            lo_subcomponent_record ?= <lfs_subcomponent>-class.
            EXIT.
          ENDIF.
        CATCH cx_root.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
    TRY.
        cl_gui_control=>set_focus( <lfs_subcomponent>-class->g_container ).
        CASE mo_case_win->g_mode.
          WHEN cl_scmg_case_visualization_win=>if_scmg_sp_case_client~c_mode_display." 'D'.
            DATA(lv_activity) = if_srm_activity_list=>display.
          WHEN cl_scmg_case_visualization_win=>if_scmg_sp_case_client~c_mode_modify."'M'.
            lv_activity = if_srm_activity_list=>modify.
          WHEN cl_scmg_case_visualization_win=>if_scmg_sp_case_client~c_mode_create."'C'.
            lv_activity = if_srm_activity_list=>create.
        ENDCASE.
        lo_subcomponent_record->send_request( im_activity = lv_activity
                                              im_poid     = lo_record_poid ).
      CATCH cx_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD setattributes.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
    DATA:
      lf_date_c  TYPE char10,
      lf_date    TYPE sy-datum,
      ls_attrval TYPE zgcm_rm_attr_val_s,
      lt_gcmattr TYPE zgcm_rm_type_attr_t,
      lt_cmattr  TYPE ty_namevalueasstring.
    DATA lv_guid    TYPE scmg_case_guid.

    lv_guid = mo_case->get_guid( ).

    SELECT SINGLE ext_key, category, zz_date_from, resp_pernr
      FROM zcmv_caseattr_d
      INTO @DATA(ls_case_attr)
      WHERE case_guid EQ @lv_guid.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    me->ms_record-doc_type =
      SWITCH #( ls_case_attr-category
        WHEN cv_case_category_ms01 THEN cv_gcm_doctype_ms01
        WHEN cv_case_category_ms02 THEN cv_gcm_doctype_ms02 ).
    me->ms_record-name     = 'Печатная форма взаимозачета'(002).
    me->ms_record-descr    = me->ms_record-name && ` ` && ls_case_attr-ext_key.

    IF me->ms_record-doc_type IS INITIAL.
      RETURN.
    ENDIF.

    SELECT attr_name
           attr_type
      FROM zgcm_rm_typeattr
      INTO CORRESPONDING FIELDS OF TABLE lt_gcmattr
      WHERE doc_type EQ me->ms_record-doc_type.
    CHECK sy-subrc EQ 0.

    lt_cmattr = me->getattributes( ).

    FREE me->mt_attrval.
    LOOP AT lt_gcmattr ASSIGNING FIELD-SYMBOL(<lfs_gcmattr>).

      ls_attrval-attr_name = <lfs_gcmattr>-attr_name.
      ls_attrval-attr_ind  = 0.

      CASE <lfs_gcmattr>-attr_name.
        WHEN 'rs_sostavitel'.
          ls_attrval-attr_value =
            SWITCH #( ls_case_attr-category
              WHEN cv_case_category_ms01 THEN get_attribut( 'CHANGED_BY' )
              WHEN cv_case_category_ms02 THEN ls_case_attr-resp_pernr ).
        WHEN 's_nazvanie_vl'.
          ls_attrval-attr_value =
            SWITCH #( ls_case_attr-category
              WHEN cv_case_category_ms01 THEN 'Соглашение о взаимозачёте'(006)
              WHEN cv_case_category_ms02 THEN 'Заявление о взаимозачёте'(008)
              ELSE '' ).
        WHEN 's_nomer_dokumenta'.
          ls_attrval-attr_value = ls_case_attr-ext_key.
        WHEN 's_reg_nomer_doka'.
          ls_attrval-attr_value = ls_case_attr-ext_key.
        WHEN 's_reg_nomer_dokumenta'.
          ls_attrval-attr_value = ls_case_attr-ext_key.
        WHEN 's_proekt'.
          ls_attrval-attr_value = ls_case_attr-ext_key.
        WHEN 's_tematika'.
          ls_attrval-attr_value =
            SWITCH #( ls_case_attr-category
              WHEN cv_case_category_ms01 THEN 'Соглашение о взаимозачёте'(006)
              WHEN cv_case_category_ms02 THEN 'Заявление о взаимозачёте'(008)
              ELSE '' ).
        WHEN 't_data_dokumenta'.
          ls_attrval-attr_value = ls_case_attr-zz_date_from.
        WHEN 't_data_registracii_doka'.
          ls_attrval-attr_value = ls_case_attr-zz_date_from.
      ENDCASE.

      IF <lfs_gcmattr>-attr_type = 'TIME'.
        IF strlen( ls_attrval-attr_value ) EQ 8 AND
           ls_attrval-attr_value(8) CO '0123456789'.
          MOVE ls_attrval-attr_value(8) TO lf_date.
          cl_reca_date=>convert_date_to_string(
            EXPORTING id_date        = lf_date
            IMPORTING ed_date_string = ls_attrval-attr_value ).
        ENDIF.
      ENDIF.

      INSERT ls_attrval INTO TABLE me->mt_attrval.
    ENDLOOP.
  ENDMETHOD.


  METHOD test_api.
*======================================================================*
* Наименование   : Формирование карточки
* Описание       : Формирование карточки
* Проект         : TRM - Трансформация КИСУ ФХД
* Постановщик    : Ковальчук Я.Г.
* Разработчик    : Козодой С.В.
* Дата создания  : 21.09.2021
* Основание      : Спецификация КИКФ_СПР_FM_F_0082
*======================================================================*
*    DATA: lv_func_name TYPE rs38l_fnam VALUE 'Z_CA_GCM_DELETE_DOCUMENT'.
*
*    TYPES BEGIN OF lty_gcm_bukrs.
*    TYPES record_id TYPE zgcm_rm_record-record_id.
*    TYPES doc_type  TYPE zgcm_rm_record-doc_type.
*    TYPES document_id TYPE zgcm_rm_record-document_id.
*    TYPES END OF lty_gcm_bukrs.
*
*    TYPES BEGIN OF lty_bor_records.
*    INCLUDE TYPE scmg_bor_in_record.
*    TYPES record_id TYPE zgcm_rm_record-record_id.
*    TYPES document_id TYPE zgcm_rm_record-document_id.
*    TYPES END OF lty_bor_records.
*
*    DATA lt_gcm_bukrs   TYPE TABLE OF lty_gcm_bukrs.
*    DATA ls_gcm_bukrs   LIKE LINE  OF lt_gcm_bukrs.
*    DATA lo_case_api    TYPE REF TO if_scmg_case_api.
*    DATA lv_bor_id      TYPE string.
*    DATA lt_bor_objects TYPE scmg_tt_bor_in_record.
*    DATA lt_bor_records TYPE TABLE OF lty_bor_records.
*    DATA: lr_record_id TYPE RANGE OF zgcm_rm_record-record_id,
*          ls_record_id LIKE LINE OF lr_record_id,
*          li_rec_api   TYPE REF TO if_srm_sp_record.
*    DATA lv_guid TYPE scmg_case_guid.
*
*    FIELD-SYMBOLS <fs_bor_objects> LIKE LINE OF lt_bor_objects.
*    FIELD-SYMBOLS <fs_bor_records> LIKE LINE OF lt_bor_records.
*
**
**    TRY. " Сохранение необходимо для передачи в API актуальных данных
**        " Но нельзя сохранять если были ошибки!!!
**        mo_case->if_scmg_case~store(
**          EXPORTING
**            im_do_validation = 'X'    " Boolean: Possible Values IF_SRM=>TRUE / FALSE
**        ).
**      CATCH cx_srm_framework.    " Framework (abstract)
**      CATCH cx_scmg.    " General Exception of Case Management
**      CATCH cx_srm_sp_client.    " Exception from SP Client
**    ENDTRY.
*
*    zcl_fm_scmg_services=>open_case(
*        EXPORTING
*          im_enqueue            = abap_true
*          im_case_guid          = mo_case->get_guid( )
*        RECEIVING
*          re_case               = lo_case_api
*        EXCEPTIONS
*          failed                = 1
*          enqueue_failed        = 2
*          invalid_guid          = 3
*          cx_srm_gsp_back       = 4
*          no_authority          = 5
*          illegal_case_type     = 6
*          OTHERS                = 7
*          ).
*
*    IF sy-subrc <> 0 OR lo_case_api IS INITIAL.
*      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*                 DISPLAY LIKE sy-msgty.
*      RETURN.
*    ENDIF.
*
*    CALL METHOD lo_case_api->bor_objects_get
*      EXPORTING
*        im_anchor      = cv_anchor_gcm_agr
*        im_bor_type    = cv_bor_type_zca_gcmrec
*      RECEIVING
*        re_bor_objects = lt_bor_objects
*      EXCEPTIONS
*        failed         = 1
*        OTHERS         = 2.
*
*    IF sy-subrc <> 0.
*      zcl_fm_scmg_services=>close_case( lo_case_api ).
*      RETURN.
*    ENDIF.
*
*    DATA lv_description TYPE string.
*    lv_description = 'Карточка ГКМ'(004) && iv_recid.
*
*    lo_case_api->bor_object_insert(
*    EXPORTING
*      im_anchor   = cv_anchor_gcm_agr
*      im_bor_key  = CONV #( iv_recid )
*      im_bor_type = cv_bor_type_zca_gcmrec
*      im_description = lv_description
*      im_sps_id = cv_sps_id_gcmrec_agr
*    EXCEPTIONS
*          failed     = 1
*          OTHERS     = 2 ).
*    IF sy-subrc NE 0.
*      zcl_fm_scmg_services=>close_case( lo_case_api ).
*      RETURN.
*    ENDIF.
*
*    CALL METHOD lo_case_api->save
*      EXPORTING
*        im_dequeue = abap_true
*      EXCEPTIONS
*        failed     = 1
*        OTHERS     = 2.
*    IF sy-subrc NE 0.
*      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*                 DISPLAY LIKE sy-msgty.
*      zcl_fm_scmg_services=>close_case( lo_case_api ).
*      RETURN.
*    ENDIF.
*
*    zcl_fm_scmg_services=>close_case( lo_case_api ).
*
*    refresh_case_interface( ).

  ENDMETHOD.
ENDCLASS.
