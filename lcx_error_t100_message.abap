CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES if_t100_message.
    METHODS constructor IMPORTING iv_id    TYPE symsgid   OPTIONAL
                                  iv_no    TYPE symsgno   OPTIONAL
                                  iv_text1 TYPE csequence OPTIONAL
                                  iv_text2 TYPE csequence OPTIONAL
                                  iv_text3 TYPE csequence OPTIONAL
                                  iv_text4 TYPE csequence OPTIONAL.
    METHODS get_bapiret2
      IMPORTING iv_msgty           TYPE sy-msgty DEFAULT 'E'
      RETURNING VALUE(rs_bapiret2) TYPE bapiret2.

    DATA mv_text1 TYPE c LENGTH 50.
    DATA mv_text2 TYPE c LENGTH 50.
    DATA mv_text3 TYPE c LENGTH 50.
    DATA mv_text4 TYPE c LENGTH 50.
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

   METHOD constructor.
    " Конструктор
    super->constructor( ).
    IF iv_id IS SUPPLIED.
      me->mv_text1 = iv_text1.
      me->mv_text2 = iv_text2.
      me->mv_text3 = iv_text3.
      me->mv_text4 = iv_text4.
      if_t100_message~t100key-msgid = iv_id.
      if_t100_message~t100key-msgno = iv_no.
    ELSE.
      me->mv_text1 = sy-msgv1.
      me->mv_text2 = sy-msgv2.
      me->mv_text3 = sy-msgv3.
      me->mv_text4 = sy-msgv4.
      if_t100_message~t100key-msgid = sy-msgid.
      if_t100_message~t100key-msgno = sy-msgno.
    ENDIF.

    if_t100_message~t100key-attr1 = 'MV_TEXT1'.
    if_t100_message~t100key-attr2 = 'MV_TEXT2'.
    if_t100_message~t100key-attr3 = 'MV_TEXT3'.
    if_t100_message~t100key-attr4 = 'MV_TEXT4'.
  ENDMETHOD.
  
  METHOD get_bapiret2.
    " Вернуть ошибку в виде строки формата bapiret2
    rs_bapiret2-id     = if_t100_message~t100key-msgid.
    rs_bapiret2-number = if_t100_message~t100key-msgno.
    rs_bapiret2-type   = iv_msgty.
    rs_bapiret2-message_v1 = mv_text1.
    rs_bapiret2-message_v2 = mv_text2.
    rs_bapiret2-message_v3 = mv_text3.
    rs_bapiret2-message_v4 = mv_text4.
    rs_bapiret2-message = me->get_text( ).
  ENDMETHOD.
ENDCLASS.
