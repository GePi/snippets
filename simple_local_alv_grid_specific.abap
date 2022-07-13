 CLASS lcl_viewer1 DEFINITION INHERITING FROM lcl_alv_viewer FINAL.
   PUBLIC SECTION.
     METHODS:
       constructor
         IMPORTING io_data TYPE REF TO lcl_data1,
     display.

     METHODS sort_data REDEFINITION.
     METHODS fill_fcat REDEFINITION.
     METHODS fill_layout REDEFINITION.
     METHODS handle_user_command REDEFINITION.
     METHODS handle_toolbar REDEFINITION.
     METHODS handle_double_click REDEFINITION.
 ENDCLASS.                    "lcl_viewer1 DEFINITION
 
 CLASS lcl_viewer1 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_controller = io_data ).
  ENDMETHOD.                    "constructor

  METHOD display.
    CALL SCREEN 0200.
  ENDMETHOD.                    "display

  METHOD sort_data.
  ENDMETHOD.                    "sort_data

  METHOD fill_fcat.
    FIELD-SYMBOLS <fs_fcat> LIKE LINE OF rt_fcat.

    rt_fcat = super->fill_fcat( ).

    LOOP AT rt_fcat ASSIGNING <fs_fcat>.
      CASE <fs_fcat>-fieldname .
        WHEN 'BUKRS' OR 'ANLN2' OR 'ZZ_SPO_NORM' OR
          'ZEILE' OR 'SJAHR' OR 'SMBLN' OR 'SMBLP'.
          <fs_fcat>-tech = abap_true.
        WHEN 'WERKS'. "Завод

        WHEN 'NAME1'. "Наименование Завода
          <fs_fcat>-scrtext_s = text-f01.
          <fs_fcat>-scrtext_m = text-f02.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f03.
        WHEN 'AUFNR1'. "Номер ведомости
          <fs_fcat>-scrtext_s = text-f04.
          <fs_fcat>-scrtext_m = text-f05.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f06.
        WHEN 'ZZ_SP_DATU'. "Дата отпуска со склада
          <fs_fcat>-scrtext_s = text-f07.
          <fs_fcat>-scrtext_m = text-f08.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f09.
        WHEN 'ZZ_KOSTL_SPO'. "Подразделение
          <fs_fcat>-scrtext_s = text-f10.
          <fs_fcat>-scrtext_m = text-f11.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f12.
        WHEN 'LTEXT'. "Наименование подразделения
          <fs_fcat>-scrtext_s = text-f16.
          <fs_fcat>-scrtext_m = text-f17.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f18.
        WHEN 'LGORT'. "Склад
          <fs_fcat>-scrtext_s = text-f19.
          <fs_fcat>-scrtext_m = text-f20.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f21.
        WHEN 'LGOBE'. "Наименование склада
          <fs_fcat>-scrtext_s = text-f22.
          <fs_fcat>-scrtext_m = text-f23.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f24.
        WHEN 'ZZ_PERNR'. "Материально-ответственное лицо
          <fs_fcat>-scrtext_s = text-f25.
          <fs_fcat>-scrtext_m = text-f26.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f27.
        WHEN 'ENAME1'. "ФИО МОЛа
          <fs_fcat>-scrtext_s = text-f28.
          <fs_fcat>-scrtext_m = text-f29.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f30.
        WHEN 'ZZ_SPO_NORM_TEXT'. "По норме/сверх норм
          <fs_fcat>-scrtext_s = text-f31.
          <fs_fcat>-scrtext_m = text-f32.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f33.
        WHEN 'PERNR'. "Сотрудник
          <fs_fcat>-scrtext_s = text-f34.
          <fs_fcat>-scrtext_m = text-f35.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f36.
        WHEN 'ENAME2'. "ФИО сотрудника
          <fs_fcat>-scrtext_s = text-f37.
          <fs_fcat>-scrtext_m = text-f38.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f39.
        WHEN 'MATNR'. "Номер материала
          <fs_fcat>-scrtext_s = text-f40.
          <fs_fcat>-scrtext_m = text-f41.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f42.
        WHEN 'MAKTX1'. "Наименование материала
          <fs_fcat>-scrtext_s = text-f43.
          <fs_fcat>-scrtext_m = text-f44.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f45.
        WHEN 'ZZETALONMAT'. "Эталонный материал
          <fs_fcat>-scrtext_s = text-f46.
          <fs_fcat>-scrtext_m = text-f47.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f48.
        WHEN 'MAKTX2'. "Наименование эталонного материала
          <fs_fcat>-scrtext_s = text-f49.
          <fs_fcat>-scrtext_m = text-f50.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f51.
        WHEN 'AQUAN'. "Количество
          <fs_fcat>-scrtext_s = text-f52.
          <fs_fcat>-scrtext_m = text-f53.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f54.
        WHEN 'MBLNR'. "Документ материала
          <fs_fcat>-scrtext_s = text-f55.
          <fs_fcat>-scrtext_m = text-f56.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f57.
        WHEN 'MJAHR'. "Год документа материала
          <fs_fcat>-scrtext_s = text-f58.
          <fs_fcat>-scrtext_m = text-f59.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f60.
        WHEN 'ANLN1'. "Основное средство
          <fs_fcat>-scrtext_s = text-f61.
          <fs_fcat>-scrtext_m = text-f62.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f63.
        WHEN 'AUFNR2'. "Заказ
          <fs_fcat>-scrtext_s = text-f64.
          <fs_fcat>-scrtext_m = text-f65.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-f66.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.                    "fill_fcat

  METHOD fill_layout.
    rs_layout = super->fill_layout( ).
    rs_layout-info_fname = 'ROWCOLOR'.
  ENDMETHOD.                    "fill_layout

  METHOD handle_user_command.
    DATA lo_excel TYPE REF TO lcl_excel1.
    CASE e_ucomm.
      WHEN cs_ui_func-download_to_excel.
        CREATE OBJECT lo_excel
          EXPORTING
            io_controller = mo_controller.
        lo_excel->display( ).
    ENDCASE.
  ENDMETHOD.                    "handle_user_command

  METHOD handle_toolbar.
    DATA ls_toolbar LIKE LINE OF e_object->mt_toolbar.

    ls_toolbar-function = cs_ui_func-download_to_excel.
    ls_toolbar-icon = icon_xxl.
    ls_toolbar-quickinfo = 'Выгрузить в Excel'(i01).
    ls_toolbar-text = ''.
    INSERT ls_toolbar INTO TABLE e_object->mt_toolbar.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_double_click.

    FIELD-SYMBOLS <ft_tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <fs_tab> TYPE any.
    FIELD-SYMBOLS <fs_field_val> TYPE any.
    FIELD-SYMBOLS <fs_field_dat> TYPE any.
    FIELD-SYMBOLS <fs_field_buk> TYPE any.

    ASSIGN md_data_table->* TO <ft_tab>.
    CHECK <ft_tab> IS ASSIGNED.

    READ TABLE <ft_tab> ASSIGNING <fs_tab> INDEX e_row-index.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE e_column-fieldname.

      WHEN 'MATNR' OR 'ZZETALONMAT'.
        ASSIGN COMPONENT e_column-fieldname OF STRUCTURE <fs_tab> TO <fs_field_val>.
        IF sy-subrc = 0.
          lcl_f2=>f2_mm03( EXPORTING iv_matnr = <fs_field_val> ).
        ENDIF.

      WHEN 'MBLNR' OR 'MJAHR'.
        ASSIGN COMPONENT 'MBLNR' OF STRUCTURE <fs_tab> TO <fs_field_val>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        ASSIGN COMPONENT 'MJAHR' OF STRUCTURE <fs_tab> TO <fs_field_dat>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        lcl_f2=>f2_migo( EXPORTING iv_mblnr = <fs_field_val>
                                   iv_mjahr = <fs_field_dat> ).

      WHEN 'ANLN1' OR 'ANLN2' OR 'BUKRS'.
        ASSIGN COMPONENT 'ANLN1' OF STRUCTURE <fs_tab> TO <fs_field_val>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_tab> TO <fs_field_buk>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        lcl_f2=>f2_as03( EXPORTING iv_anln1 = <fs_field_val>
                                   iv_bukrs = <fs_field_buk> ).

      WHEN 'AUFNR1' OR 'AQUAN'.
        ASSIGN COMPONENT 'AUFNR1' OF STRUCTURE <fs_tab> TO <fs_field_val>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        ASSIGN COMPONENT 'ZZ_SP_DATU' OF STRUCTURE <fs_tab> TO <fs_field_dat>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        lcl_f2=>f2_z_spo( EXPORTING iv_aufnr = <fs_field_val> iv_date = <fs_field_dat> ).

      WHEN 'AUFNR2'.
        ASSIGN COMPONENT e_column-fieldname OF STRUCTURE <fs_tab> TO <fs_field_val>.
        IF sy-subrc = 0.
          lcl_f2=>f2_ko03( EXPORTING iv_aufnr = <fs_field_val> ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "handle_double_click

ENDCLASS.                    "lcl_qrmin_viewer IMPLEMENTATION
