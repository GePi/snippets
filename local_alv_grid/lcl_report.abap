CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      main IMPORTING io_param TYPE REF TO lcl_param
           RETURNING value(ro_report) TYPE REF TO lcl_report,

     at_ss_output,

     get_title RETURNING value(rv_title) TYPE string,
     get_pfs   RETURNING value(rv_pfs) TYPE string,
     pbo_0200_status,
     pbo_0200_controls,
     pai_0200_exit_command
        CHANGING cv_ok TYPE sy-ucomm,
     pai_0200_user_command
        CHANGING cv_ok TYPE sy-ucomm.

    CLASS-DATA mo_param TYPE REF TO lcl_param READ-ONLY.

  PRIVATE SECTION.
    METHODS:
      select_data,
      display,
      set_param IMPORTING io_param TYPE REF TO lcl_param.
    CLASS-DATA:
      mo_report TYPE REF TO lcl_report,

      " Ведомость выдачи СИЗ
      mo_data1  TYPE REF TO lcl_data1,
      mo_viewer1 TYPE REF TO lcl_viewer1,
      " Ведомость возврата СИЗ
      mo_data2  TYPE REF TO lcl_data2,
      mo_viewer2 TYPE REF TO lcl_viewer2,
      " СКЗ в эксплуатации
      mo_data3  TYPE REF TO lcl_data3,
      mo_viewer3 TYPE REF TO lcl_viewer3,
      " Выгрузка изменений по складам
      mo_data4  TYPE REF TO lcl_data4,
      mo_viewer4 TYPE REF TO lcl_viewer4.
ENDCLASS.                    "lcl_report DEFINITION


CLASS lcl_report IMPLEMENTATION.
  METHOD main.
    " Выполнить отчет
    CREATE OBJECT mo_report.

    mo_report->set_param( io_param ).

    mo_report->select_data( ).

    mo_report->display( ).
  ENDMETHOD.                    "main

METHOD select_data.
    " Выбрать данные
    CASE mo_param->mv_repnum.
      WHEN mo_param->cs_repnum-r1.
        CREATE OBJECT mo_data1
          EXPORTING
            io_param = mo_param.
        mo_data1->select( ).
      WHEN mo_param->cs_repnum-r2.
        CREATE OBJECT mo_data2
          EXPORTING
            io_param = mo_param.
        mo_data2->select( ).
      WHEN mo_param->cs_repnum-r3.
        CREATE OBJECT mo_data3
          EXPORTING
            io_param = mo_param.
        mo_data3->select( ).
      WHEN mo_param->cs_repnum-r4.
        CREATE OBJECT mo_data4
          EXPORTING
            io_param = mo_param.
        mo_data4->select( ).
    ENDCASE.
  ENDMETHOD.                    "select_data

  METHOD display.
    " Отобразить данные

    CASE mo_param->mv_repnum.
      WHEN mo_param->cs_repnum-r1.
        CREATE OBJECT mo_viewer1
          EXPORTING
            io_data = mo_data1.
        CALL SCREEN 0200.

      WHEN mo_param->cs_repnum-r2.
        CREATE OBJECT mo_viewer2
          EXPORTING
            io_data = mo_data2.
        CALL SCREEN 0200.

      WHEN mo_param->cs_repnum-r3.
        CREATE OBJECT mo_viewer3
          EXPORTING
            io_data = mo_data3.
        CALL SCREEN 0200.

      WHEN mo_param->cs_repnum-r4.
        CREATE OBJECT mo_viewer4
          EXPORTING
            io_data = mo_data4.
        CALL SCREEN 0200.
    ENDCASE.
  ENDMETHOD.                    "display
METHOD get_title.
    " Получить заголовок экрана
    CASE mo_param->mv_repnum.
      WHEN mo_param->cs_repnum-r1.
        rv_title = 'Ведомость выдачи СИЗ'(t01).
      WHEN mo_param->cs_repnum-r2.
        rv_title = 'Выгрузка даных по возвратам СИЗ'(t02).
      WHEN mo_param->cs_repnum-r3.
        rv_title = 'Выгрузка даных СКЗ в эксплуатации'(t03).
      WHEN mo_param->cs_repnum-r4.
        rv_title = 'Выгрузка списка складов'(t04).
    ENDCASE.

  ENDMETHOD.                    "get_title

  METHOD get_pfs.
    " Получить статус
    rv_pfs = 'PFS_0200_REPORT'.
  ENDMETHOD.                    "get_title

  METHOD pbo_0200_status.
    DATA lv_str TYPE string.
    lv_str = lcl_report=>get_title( ).
    SET TITLEBAR 'TB_0200_REPORT' WITH lv_str.
    lv_str = lcl_report=>get_pfs( ).
    SET PF-STATUS lv_str.
  ENDMETHOD.                    "pbo_0200_status

  METHOD pbo_0200_controls.
    " PBO 200 экрана - управление контролами
    CASE mo_param->mv_repnum.
      WHEN mo_param->cs_repnum-r1.
        mo_viewer1->control_init( ).
      WHEN mo_param->cs_repnum-r2.
        mo_viewer2->control_init( ).
      WHEN mo_param->cs_repnum-r3.
        mo_viewer3->control_init( ).
      WHEN mo_param->cs_repnum-r4.
        mo_viewer4->control_init( ).
    ENDCASE.
  ENDMETHOD.                    "pbo_0200_controls

  METHOD pai_0200_exit_command.

    CASE mo_param->mv_repnum.
      WHEN mo_param->cs_repnum-r1.
        mo_viewer1->free( ).
        FREE mo_viewer1.
        FREE mo_data1.
      WHEN mo_param->cs_repnum-r2.
        mo_viewer2->free( ).
        FREE mo_viewer2.
        FREE mo_data2.
      WHEN mo_param->cs_repnum-r3.
        mo_viewer3->free( ).
        FREE mo_viewer3.
        FREE mo_data3.
      WHEN mo_param->cs_repnum-r4.
        mo_viewer4->free( ).
        FREE mo_viewer4.
        FREE mo_data4.
    ENDCASE.

    IF cv_ok = 'EXIT' OR cv_ok = 'STOP'.
      LEAVE PROGRAM.
    ENDIF.
  ENDMETHOD.                    "pai_0200_exit_command

  METHOD pai_0200_user_command.
    IF cv_ok = 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    ENDIF.
  ENDMETHOD.                    "pai_0200_user_command
  ENDCLASS.                    "lcl_report IMPLEMENTATION
