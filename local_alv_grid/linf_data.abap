INTERFACE linf_data.
  METHODS get_ref_data RETURNING value(rd_data) TYPE REF TO data.
ENDINTERFACE.                    "lcl_param DEFINITION

CLASS lcl_data DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES linf_data ALL METHODS ABSTRACT.

    CONSTANTS mc_aufk_auart_cspe TYPE aufk-auart VALUE 'CSPE'.
    DATA mo_param TYPE REF TO lcl_param READ-ONLY.
ENDCLASS.                    "lcl_data DEFINITION

CLASS lcl_data1 DEFINITION FINAL INHERITING FROM lcl_data.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_data,
             END OF ts_data,
      tt_data TYPE STANDARD TABLE OF ts_data WITH DEFAULT KEY.

    METHODS linf_data~get_ref_data REDEFINITION.

    DATA mt_data TYPE tt_data.
ENDCLASS.                    "lcl_data1 DEFINITION

  METHOD linf_data~get_ref_data.
    " Получить ссылку на таблицу с данными
    GET REFERENCE OF mt_data INTO rd_data.
  ENDMETHOD.                    "get_ref_to_data
