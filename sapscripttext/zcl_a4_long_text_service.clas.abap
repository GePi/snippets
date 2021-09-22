CLASS zcl_a4_long_text_service DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS mc_plain_text_length TYPE i VALUE 255 ##NO_TEXT.

    TYPES:
      ts_plain TYPE c LENGTH mc_plain_text_length .
    TYPES:
      tt_plain TYPE STANDARD TABLE OF ts_plain WITH DEFAULT KEY .

    CLASS-METHODS load
      IMPORTING
        !is_header       TYPE thead
      RETURNING
        VALUE(rt_values) TYPE tline_t .
    CLASS-METHODS save
      IMPORTING
        !is_header TYPE thead
        !it_values TYPE tline_t .
    CLASS-METHODS delete
      IMPORTING
        !is_header TYPE thead .
    CLASS-METHODS string_to_plain
      IMPORTING
        !iv_value        TYPE clike
      RETURNING
        VALUE(rt_values) TYPE tt_plain .
    CLASS-METHODS string_to_lines
      IMPORTING
        !iv_value        TYPE clike
      RETURNING
        VALUE(rt_values) TYPE tline_t .
    CLASS-METHODS string_table_to_lines
      IMPORTING
        !it_values       TYPE za4t_string_t
      RETURNING
        VALUE(rt_values) TYPE tline_t .
    CLASS-METHODS plain_to_lines
      IMPORTING
        !it_values       TYPE tt_plain
      RETURNING
        VALUE(rt_values) TYPE tline_t .
    CLASS-METHODS plain_to_string
      IMPORTING
        !it_values      TYPE tt_plain
      RETURNING
        VALUE(rv_value) TYPE string .
    CLASS-METHODS lines_to_plain
      IMPORTING
        !it_values       TYPE tline_t
      RETURNING
        VALUE(rt_values) TYPE tt_plain .
    CLASS-METHODS lines_to_string
      IMPORTING
        !it_values      TYPE tline_t
      RETURNING
        VALUE(rv_value) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_A4_LONG_TEXT_SERVICE IMPLEMENTATION.


  METHOD delete.

    CALL FUNCTION 'DELETE_TEXT'
      EXPORTING
        id              = is_header-tdid
        language        = is_header-tdspras
        name            = is_header-tdname
        object          = is_header-tdobject
        savemode_direct = abap_true
      EXCEPTIONS
        OTHERS          = 1.
    CHECK sy-subrc = 0.

    CALL FUNCTION 'COMMIT_TEXT'
      EXPORTING
        object          = is_header-tdobject
        name            = is_header-tdname
        savemode_direct = abap_true.
  ENDMETHOD.


  METHOD lines_to_plain.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      TABLES
        itf_text    = it_values
        text_stream = rt_values.
  ENDMETHOD.


  METHOD lines_to_string.

    rv_value = plain_to_string( lines_to_plain( it_values ) ).
  ENDMETHOD.


  METHOD load.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = is_header-tdid
        language = is_header-tdspras
        name     = is_header-tdname
        object   = is_header-tdobject
      TABLES
        lines    = rt_values
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc NE 0.
      CLEAR rt_values.
    ENDIF.
  ENDMETHOD.


  METHOD plain_to_lines.

    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      TABLES
        text_stream = it_values
        itf_text    = rt_values.
  ENDMETHOD.


  METHOD plain_to_string.

    CALL FUNCTION 'CONVERT_TABLE_TO_STRING'
      EXPORTING
        i_tabline_length = mc_plain_text_length
      IMPORTING
        e_string         = rv_value
      TABLES
        it_table         = it_values.
  ENDMETHOD.


  METHOD save.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header = is_header
      TABLES
        lines  = it_values
      EXCEPTIONS
        OTHERS = 1.
    CHECK sy-subrc = 0.

    CALL FUNCTION 'COMMIT_TEXT'
      EXPORTING
        object          = is_header-tdobject
        name            = is_header-tdname
        savemode_direct = abap_true.
  ENDMETHOD.


  METHOD string_table_to_lines.

    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        stream_lines = it_values
        lf           = 'X'
      TABLES
        itf_text     = rt_values.
  ENDMETHOD.


  METHOD string_to_lines.

    rt_values = plain_to_lines( string_to_plain( iv_value ) ).
  ENDMETHOD.


  METHOD string_to_plain.

    DATA:

      lv_value TYPE string
    .

    lv_value = iv_value.
    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = lv_value
      TABLES
        ftext_tab = rt_values.
  ENDMETHOD.
ENDCLASS.
