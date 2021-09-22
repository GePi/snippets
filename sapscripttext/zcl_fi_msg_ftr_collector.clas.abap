class ZCL_FI_MSG_FTR_COLLECTOR definition
  public
  final
  create public .

public section.

  interfaces ZIF_FI_MSG_PROXY .

  methods CONSTRUCTOR
    importing
      !IO_TRTM_PROXY_MESSAGE type ref to IF_OPEN_TRTM_PROXY_MESSAGE .
protected section.
private section.

  data MO_TRTM_PROXY_MESSAGE type ref to IF_OPEN_TRTM_PROXY_MESSAGE .
ENDCLASS.



CLASS ZCL_FI_MSG_FTR_COLLECTOR IMPLEMENTATION.


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

    mo_trtm_proxy_message = io_trtm_proxy_message.
  ENDMETHOD.


  METHOD zif_fi_msg_proxy~message.
*======================================================================*
* Наименование   : METHOD zif_fi_msg_proxy~message
* Описание       : Сформировать сообщение
* Проект         : TRM - Трансформация КИСУ ПХД ПАО «ФСК ЕЭС»
* Постановщик    : Костинов М.А.
* Разработчик    : Козодой С.В.
* Дата создания  : 09.09.2021
* Основание      : Спецификация 0005
*======================================================================*

    CALL METHOD mo_trtm_proxy_message->set_message
      EXPORTING
        pi_message_id       = sy-msgid
        pi_message_number   = sy-msgno
        pi_message_severity = sy-msgty
        pi_message_var1     = sy-msgv1
        pi_message_var2     = sy-msgv2
        pi_message_var3     = sy-msgv3
        pi_message_var4     = sy-msgv4
      EXCEPTIONS
        OTHERS              = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
