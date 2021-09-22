class ZCL_FI_FTR_LONG_TEXT_EXTENSION definition
  public
  final
  create public .

public section.
  type-pools FTRG .

  types T_TEXT_DESCR type CHAR120 .
  types:
    BEGIN OF ts_text,
        tdid       TYPE tdid,
        text_descr TYPE t_text_descr,
        o_text     TYPE REF TO zcl_fi_long_text_onscreen,
      END OF ts_text .
  types:
    tt_h_text TYPE HASHED TABLE OF ts_text WITH UNIQUE KEY tdid .

  constants CC_TDID_INOE type TDID value 'INOE' ##NO_TEXT.
  constants CC_TDOBJECT_FTR_LONG_TEXT type TDOBJECT value 'ZTB_FHA' ##NO_TEXT.
  constants CC_LENGTH_SCR_FIELD type INT4 value 72 ##NO_TEXT.
  constants CC_TDID_POYA type TDID value 'POYA' ##NO_TEXT.
  constants CC_TDID_DPOY type TDID value 'DPOY' ##NO_TEXT.
  constants CC_TDID_DSRO type TDID value 'DSRO' ##NO_TEXT.
  constants CC_TDID_KOMI type TDID value 'KOMI' ##NO_TEXT.
  constants CC_TDID_KOMM type TDID value 'KOMM' ##NO_TEXT.
  class-data MV_DESCR_POYA type T_TEXT_DESCR .
  class-data MV_DESCR_DPOY type T_TEXT_DESCR .
  class-data MV_DESCR_DSRO type T_TEXT_DESCR .
  class-data MV_DESCR_KOMI type T_TEXT_DESCR .
  class-data MV_DESCR_KOMM type T_TEXT_DESCR .
  class-data MV_DESCR_INOE type T_TEXT_DESCR .

  class-methods INITIALIZATION
    importing
      !IV_BUKRS type BUKRS
      !IV_RFHA type TB_RFHA
      !IV_EDIT_MODE type FTRG_CHAR .
  class-methods SET_FROM_TEXT_FIELD
    importing
      !IV_TDID type TDID
      !IV_TEXT_FIELD_VAL type STRING .
  class-methods EDIT_TEXT
    importing
      !IV_TDID type TDID
    returning
      value(RV_SCR_FIELD_VAL) type STRING .
  class-methods CLASS_CONSTRUCTOR .
  class-methods SAVE
    importing
      !IV_BUKRS type BUKRS
      !IV_RFHA type TB_RFHA
      !IV_EDIT_MODE type FTRG_CHAR .
  class-methods GET_TO_TEXT_FIELD
    importing
      !IV_TDID type TDID
    returning
      value(RV_TEXT_FIELD_VAL) type STRING .
  class-methods CHECK_FIELD_FILLED
    importing
      !IO_MSG_PROXY type ref to ZIF_FI_MSG_PROXY .
protected section.
private section.

  class-data MV_EDIT_MODE type FTRG_CHAR .
  class-data MT_TEXT type TT_H_TEXT .
  class-data MS_TTXOB type TTXOB .
  class-data CC_TDLINESIZE_DEFAULT type TDLINESIZE value '072' ##NO_TEXT.
  class-data CC_LANG_RU type SY-LANGU value 'R' ##NO_TEXT.

  class-methods GET_SCREEN_OBJ
    importing
      !IV_TDID type TDID
    returning
      value(RO_LONG_TEXT_ONSCREEN) type ref to ZCL_FI_LONG_TEXT_ONSCREEN .
ENDCLASS.



CLASS ZCL_FI_FTR_LONG_TEXT_EXTENSION IMPLEMENTATION.


  METHOD check_field_filled.
*======================================================================*
* Наименование   : METHOD check_field_filled
* Описание       : Проверить заполнены ли требуемые текстовые поля
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    IF mv_edit_mode = if_ftr_con=>editmode_display.
      RETURN.
    ENDIF.

    LOOP AT mt_text ASSIGNING FIELD-SYMBOL(<fs_text>).

      IF <fs_text>-o_text IS NOT BOUND.
        CONTINUE.
      ENDIF.
      DATA(lo_text) = <fs_text>-o_text.

      IF lo_text->is_empty( ).
        " Заполните обязательное текстовое поле: &1
        MESSAGE e001(zfi_ftr_customer) WITH <fs_text>-text_descr INTO DATA(lv_dummy).
        io_msg_proxy->message( ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD class_constructor.
*======================================================================*
* Наименование   : METHOD class_constructor.
* Описание       : Статический конструктор класса
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    mv_descr_poya	= 'Пояснение (полностью пункт договора)'(t01).
    mv_descr_dpoy	= 'Пояснение (вписать пункт договора)'(t02).
    mv_descr_dsro	=	'Срок уведомления при досрочном погашении'(t03).
    mv_descr_komi	= 'Прочие комиссии'(t04).
    mv_descr_komm	=	'Комментарий'(t05).
    mv_descr_inoe	= 'Иное'(t06).

    " Заголовок объекта текстов
    SELECT SINGLE *
      INTO ms_ttxob
      FROM ttxob
      WHERE tdobject = cc_tdobject_ftr_long_text.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " Значения по умолчанию
    ms_ttxob-tdlinesize = cc_tdlinesize_default.

  ENDMETHOD.


  METHOD edit_text.
*======================================================================*
* Наименование   : METHOD edit_text
* Описание       : Редактировать текст
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    DATA(lo_text) = get_screen_obj( iv_tdid ).

    IF lo_text IS NOT BOUND.
      RETURN.
    ENDIF.

    lo_text->edit_text(
      iv_display = SWITCH flag( mv_edit_mode
         WHEN if_ftr_con=>editmode_create THEN abap_false
         WHEN if_ftr_con=>editmode_change THEN abap_false
         ELSE abap_true
         ) ).

    " Начало текста для экранного поля
    rv_scr_field_val = lo_text->get_to_text_feild( ).

  ENDMETHOD.


  METHOD get_screen_obj.
*======================================================================*
* Наименование   : METHOD get_screen_obj
* Описание       : Получить объект  для обрабатываемого tdid текста
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    TRY.
        ro_long_text_onscreen = mt_text[ tdid = iv_tdid ]-o_text.
      CATCH cx_sy_itab_line_not_found.
        ro_long_text_onscreen = NEW #(
          is_header =
            VALUE #( tdobject = cc_tdobject_ftr_long_text
                     tdid     = cc_tdid_inoe
                     tdspras  = 'R'
                     tdlinesize = ms_ttxob-tdlinesize )
          iv_length_scr_field = cc_length_scr_field
        ).
        INSERT VALUE #( tdid = iv_tdid o_text = ro_long_text_onscreen )
          INTO TABLE mt_text.
    ENDTRY.

  ENDMETHOD.


  METHOD get_to_text_field.
*======================================================================*
* Наименование   : METHOD get_to_text_field
* Описание       : Получить значение для экранного поля
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    DATA(lo_text) = get_screen_obj( iv_tdid ).

    IF lo_text IS NOT BOUND.
      RETURN.
    ENDIF.

    rv_text_field_val = lo_text->get_to_text_feild( ).
  ENDMETHOD.


  METHOD initialization.
*======================================================================*
* Наименование   : METHOD initialization
* Описание       : Инициализировать таблицу текстов
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    DATA lv_tdname TYPE tdobname.
    " Ключ текста
    lv_tdname = iv_bukrs && iv_rfha.
    " Текст Иное
    INSERT VALUE #( tdid = cc_tdid_inoe
                    text_descr = mv_descr_inoe
                    o_text = NEW #(
                      is_header =
                        VALUE #( tdobject = cc_tdobject_ftr_long_text
                                 tdid     = cc_tdid_inoe
                                 tdspras  = cc_lang_ru
                                 tdname   = lv_tdname
                                 tdlinesize = ms_ttxob-tdlinesize )
                    iv_length_scr_field = cc_length_scr_field
                    ) ) INTO TABLE mt_text.
    " Текст Пояснение (полностью пункт договора)
    INSERT VALUE #( tdid = cc_tdid_poya
                    text_descr = mv_descr_poya
                    o_text = NEW #(
                       is_header =
                         VALUE #( tdobject = cc_tdobject_ftr_long_text
                                  tdid     = cc_tdid_poya
                                  tdspras  = cc_lang_ru
                                  tdname   = lv_tdname
                                  tdlinesize = ms_ttxob-tdlinesize )
                    iv_length_scr_field = cc_length_scr_field
                     ) ) INTO TABLE mt_text.
    " Текст Пояснение (вписать пункт договора)
    INSERT VALUE #( tdid = cc_tdid_dpoy
                    text_descr = mv_descr_dpoy
                    o_text = NEW #(
                       is_header =
                         VALUE #( tdobject = cc_tdobject_ftr_long_text
                                  tdid     = cc_tdid_dpoy
                                  tdspras  = cc_lang_ru
                                  tdname   = lv_tdname
                                  tdlinesize = ms_ttxob-tdlinesize )
                    iv_length_scr_field = cc_length_scr_field
                     ) ) INTO TABLE mt_text.
    " Текст Срок уведомления при досрочном погашении
    INSERT VALUE #( tdid = cc_tdid_dsro
                    text_descr = mv_descr_dsro
                    o_text = NEW #(
                      is_header =
                        VALUE #( tdobject = cc_tdobject_ftr_long_text
                                 tdid     = cc_tdid_dsro
                                 tdspras  = cc_lang_ru
                                 tdname   = lv_tdname
                                 tdlinesize = ms_ttxob-tdlinesize )
                    iv_length_scr_field = cc_length_scr_field
                  ) ) INTO TABLE mt_text.
    " Текст Прочие комиссии
    INSERT VALUE #( tdid = cc_tdid_komi
                    text_descr = mv_descr_komi
                    o_text = NEW #(
                      is_header =
                        VALUE #( tdobject = cc_tdobject_ftr_long_text
                                 tdid     = cc_tdid_komi
                                 tdspras  = cc_lang_ru
                                 tdname   = lv_tdname
                                 tdlinesize = ms_ttxob-tdlinesize )
                    iv_length_scr_field = cc_length_scr_field
               ) ) INTO TABLE mt_text.
    " Текст Комментарий
    INSERT VALUE #( tdid = cc_tdid_komm
                    text_descr = mv_descr_komm
                    o_text = NEW #(
                      is_header =
                        VALUE #( tdobject = cc_tdobject_ftr_long_text
                                 tdid     = cc_tdid_komm
                                 tdspras  = cc_lang_ru
                                 tdname   = lv_tdname
                                 tdlinesize = ms_ttxob-tdlinesize )
                    iv_length_scr_field = cc_length_scr_field
            ) ) INTO TABLE mt_text.

    " Режим обработки сделки
    mv_edit_mode = iv_edit_mode.

    IF iv_edit_mode = if_ftr_con=>editmode_create.
      RETURN.
    ENDIF.

    LOOP AT mt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
      <fs_text>-o_text->load( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD save.
*======================================================================*
* Наименование   : METHOD save
* Описание       : Сохранить тексты
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    DATA lv_tdname TYPE tdobname.

    CHECK iv_edit_mode = if_ftr_con=>editmode_create
       OR iv_edit_mode = if_ftr_con=>editmode_change.

    lv_tdname = iv_bukrs && iv_rfha.

    LOOP AT mt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
      <fs_text>-o_text->set_tdname( lv_tdname ).
      <fs_text>-o_text->save_changed_text( ).
    ENDLOOP.

    CALL FUNCTION 'COMMIT_TEXT'
      EXPORTING
        object = cc_tdobject_ftr_long_text.

  ENDMETHOD.


  METHOD set_from_text_field.
*======================================================================*
* Наименование   : METHOD set_from_text_field
* Описание       : Установить значение из экранного поля
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*
    DATA(lo_text) = get_screen_obj( iv_tdid ).

    IF lo_text IS NOT BOUND.
      RETURN.
    ENDIF.

    lo_text->set_from_text_field( iv_text_field_val ).
  ENDMETHOD.
ENDCLASS.
