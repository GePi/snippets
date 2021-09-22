class ZCL_FI_LONG_TEXT_ONSCREEN definition
  public
  final
  create public .

public section.

  data MV_LENGTH_SCR_FIELD type INT4 .
  data MT_LINES type TLINE_T .
  data MV_SCR_FIELD type STRING .
  data MS_HEADER type THEAD .
  data MV_FL_CHANGED type FLAG value ABAP_FALSE ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IS_HEADER type THEAD
      !IV_LENGTH_SCR_FIELD type INT4
      !IV_FL_LOAD type FLAG optional .
  methods SET_FROM_TEXT_FIELD
    importing
      !IV_NEW_TEXT_FIELD_VAL type STRING .
  methods EDIT_TEXT
    importing
      !IV_DISPLAY type FLAG default ABAP_FALSE .
  methods GET_TO_TEXT_FEILD
    returning
      value(RV_SCR_FIELD_VAL) type STRING .
  methods LOAD .
  methods SAVE .
  methods SAVE_CHANGED_TEXT .
  methods SET_TDNAME
    importing
      !IV_TDNAME type TDOBNAME .
  methods IS_EMPTY
    returning
      value(RV_FL_EMPTY) type FLAG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FI_LONG_TEXT_ONSCREEN IMPLEMENTATION.


  METHOD constructor.
*======================================================================*
* Наименование   : METHOD constructor
* Описание       : Конструктор класса
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    ms_header = is_header.
    mv_length_scr_field = iv_length_scr_field.

    IF iv_fl_load = abap_false.
      RETURN.
    ENDIF.

    mt_lines =
      zcl_a4_long_text_service=>load( is_header = ms_header ).
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

    DATA(mt_lines_old) = mt_lines.

    CALL FUNCTION 'EDIT_TEXT'
      EXPORTING
        display = iv_display
        header  = ms_header
        save    = abap_false
      TABLES
        lines   = mt_lines.

    IF mt_lines <> mt_lines_old.
      mv_fl_changed = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_to_text_feild.
*======================================================================*
* Наименование   : METHOD get_to_text_feild
* Описание       : Получить значение из начала текста для экранного поля
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    " Начало текста для экранного поля
    CLEAR rv_scr_field_val.
    IF mt_lines IS NOT INITIAL.
      rv_scr_field_val = mt_lines[ 1 ]-tdline.
      rv_scr_field_val =
        zcl_a4_string=>substring( iv_value = rv_scr_field_val
                                  iv_count = mv_length_scr_field ).
    ENDIF.
  ENDMETHOD.


  METHOD is_empty.
*======================================================================*
* Наименование   : METHOD is_empty
* Описание       : Заполнен ли текст
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    rv_fl_empty = abap_false.
    IF zcl_a4_long_text_service=>lines_to_string( mt_lines ) CO
        cl_abap_char_utilities=>get_simple_spaces_for_cur_cp( ).
      rv_fl_empty = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD load.
*======================================================================*
* Наименование   : METHOD load
* Описание       : Загрузить текст
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    mt_lines = zcl_a4_long_text_service=>load( ms_header ).
  ENDMETHOD.


  METHOD save.
*======================================================================*
* Наименование   : METHOD save
* Описание       : Сохранить текст
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = ms_header
        savemode_direct = abap_false
      TABLES
        lines           = mt_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      CLEAR mt_lines.
    ENDIF.
  ENDMETHOD.


  METHOD save_changed_text.
*======================================================================*
* Наименование   : METHOD save_changed_text
* Описание       : Сохранить текст, если он изменялся
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    IF mv_fl_changed = abap_true.
      save( ).
    ENDIF.
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

    DATA lv_old_scr_field_val TYPE char255.
    DATA lv_new_scr_field_val TYPE char255.

    IF mt_lines IS INITIAL.
      mt_lines =
        zcl_a4_long_text_service=>string_to_lines( iv_new_text_field_val ).

      mv_fl_changed = abap_true.
      RETURN.
    ENDIF.

    " Сравнение начального фрагмента текста с переданной
    " строкой - значением экранного поля
    lv_old_scr_field_val = mt_lines[ 1 ]-tdline.
    lv_new_scr_field_val = iv_new_text_field_val.

    lv_old_scr_field_val = lv_old_scr_field_val(mv_length_scr_field).
    lv_new_scr_field_val = lv_new_scr_field_val(mv_length_scr_field).

    IF lv_new_scr_field_val = lv_old_scr_field_val.
      RETURN.
    ENDIF.

    " Формирование нового текста путем заменой начального фрагмента на
    " значение экранного поля
    lv_old_scr_field_val = mt_lines[ 1 ]-tdline.

    mt_lines[ 1 ]-tdline = lv_new_scr_field_val && lv_old_scr_field_val+mv_length_scr_field.

    mv_fl_changed = abap_true.
  ENDMETHOD.


  METHOD set_tdname.
*======================================================================*
* Наименование   : METHOD set_tdname
* Описание       : Установить имя текста (поле ms_header-tdname)
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    ms_header-tdname = iv_tdname.
  ENDMETHOD.
ENDCLASS.
