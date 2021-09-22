class ZCL_FI_MSG_SCREEN definition
  public
  final
  create public .

public section.

  interfaces ZIF_FI_MSG_PROXY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FI_MSG_SCREEN IMPLEMENTATION.


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

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDMETHOD.
ENDCLASS.
