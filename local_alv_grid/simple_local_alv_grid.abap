CLASS lcl_alv_viewer DEFINITION ABSTRACT.
   PUBLIC SECTION.
     CONSTANTS:
       BEGIN OF cs_ui_func,
         download_to_excel TYPE ui_func VALUE 'EXCEL',
         download_to_xml TYPE ui_func VALUE 'XML',
       END OF cs_ui_func.

     METHODS:
       constructor IMPORTING io_controller TYPE REF TO linf_data,

       control_init,
       refresh,
       free,

       handle_toolbar ABSTRACT FOR EVENT toolbar OF cl_gui_alv_grid
         IMPORTING e_object e_interactive,
       handle_user_command ABSTRACT
                     FOR EVENT user_command OF cl_gui_alv_grid
         IMPORTING e_ucomm,
       handle_double_click ABSTRACT
                     FOR EVENT double_click OF cl_gui_alv_grid
         IMPORTING e_row e_column.
     METHODS:
       sort_data,
       fill_fcat RETURNING value(rt_fcat) TYPE lvc_t_fcat,
       fcat_copy_text_from_field
         IMPORTING iv_fieldname TYPE fieldname
                   it_fcat TYPE lvc_t_fcat
         CHANGING  cs_fcat TYPE lvc_s_fcat,
       fill_variant RETURNING value(rs_var) TYPE disvariant,
       fill_layout RETURNING value(rs_layout) TYPE lvc_s_layo,
       fill_fcode_excl RETURNING value(rt_excl) TYPE ui_functions,
       fill_mode_save RETURNING value(rv_mode_save) TYPE char01,

       set_grid_events,

       get_lvc_t_fcat_4_itab
         IMPORTING it_table       TYPE STANDARD TABLE
         RETURNING value(rt_fcat) TYPE lvc_t_fcat.

     DATA:
       mo_controller TYPE REF TO linf_data.

     DATA mo_cont TYPE REF TO cl_gui_custom_container.
     DATA mo_docking TYPE REF TO cl_gui_docking_container.
     DATA mo_grid TYPE REF TO cl_gui_alv_grid.

     DATA md_data_table TYPE REF TO data.
 ENDCLASS.                    "lcl_xxx_viewer DEFINITION
 
 
 CLASS lcl_alv_viewer IMPLEMENTATION.
  METHOD constructor.
    mo_controller = io_controller.
  ENDMETHOD.                    "constructor

  METHOD free.
    IF mo_grid IS NOT INITIAL.
      mo_grid->free( ).
    ENDIF.
    IF mo_cont IS NOT INITIAL.
      mo_cont->free( ).
    ENDIF.

    FREE mo_grid.
    FREE mo_cont.
  ENDMETHOD.                    "free

  METHOD refresh.
    IF mo_grid IS INITIAL.
      RETURN.
    ENDIF.
    mo_grid->refresh_table_display( ).
  ENDMETHOD.                    "refresh

  METHOD control_init.
    " Инициализация alv-grid
    DATA lv_style TYPE i.
    DATA:
      lt_fcat TYPE lvc_t_fcat,
      ls_variant TYPE disvariant,
      ls_layout TYPE lvc_s_layo,
      lt_fcode_excl TYPE ui_functions,
      lv_mode_save TYPE char01.

    FIELD-SYMBOLS <ft_data> TYPE STANDARD TABLE.

    IF mo_grid IS NOT INITIAL.
      mo_grid->refresh_table_display( ).
      RETURN.
    ENDIF.

    lv_style = cl_gui_control=>ws_child + cl_gui_control=>ws_visible.

    CREATE OBJECT mo_docking
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        extension = cl_gui_docking_container=>ws_maximizebox
        style     = lv_style.

    CREATE OBJECT mo_grid
      EXPORTING
        i_parent = mo_docking.

    md_data_table = mo_controller->get_ref_data( ).
    ASSIGN md_data_table->* TO <ft_data>.

    sort_data( ).

    lt_fcat = fill_fcat( ).
    ls_variant = fill_variant( ).
    ls_layout = fill_layout( ).
    lt_fcode_excl = fill_fcode_excl( ).
    lv_mode_save = fill_mode_save( ).

    set_grid_events( ).

    CALL METHOD mo_grid->set_table_for_first_display
      EXPORTING
        i_save                        = lv_mode_save
        is_variant                    = ls_variant
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_fcode_excl
      CHANGING
        it_outtab                     = <ft_data>
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
  ENDMETHOD.                    "control_init

  METHOD sort_data.
    " Сортировка данных перед выводом (сортируется сама таблица данных в памяти)
  ENDMETHOD.                    "sort_data

  METHOD fill_fcat.
    FIELD-SYMBOLS <ft_tab> TYPE STANDARD TABLE.
    ASSIGN md_data_table->* TO <ft_tab>.
    rt_fcat = get_lvc_t_fcat_4_itab( <ft_tab> ).
  ENDMETHOD.                    "fill_fcat

  METHOD fcat_copy_text_from_field.
    FIELD-SYMBOLS <fs_fcat> LIKE LINE OF it_fcat.
    READ TABLE it_fcat ASSIGNING <fs_fcat>
      WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    cs_fcat-scrtext_s = <fs_fcat>-scrtext_s.
    cs_fcat-scrtext_m = <fs_fcat>-scrtext_m.
    cs_fcat-scrtext_l = cs_fcat-coltext = <fs_fcat>-scrtext_l.
  ENDMETHOD.                    "fcat_copy_text_from_field

  METHOD fill_variant.
    rs_var-report = sy-repid.
    rs_var-username = sy-uname.
  ENDMETHOD.                    "fill_variant

  METHOD fill_layout.
    rs_layout-cwidth_opt = abap_true.
  ENDMETHOD.                    "fill_layout

  METHOD fill_fcode_excl.
    APPEND cl_gui_alv_grid=>mc_fc_info TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_print TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_graph TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_detail TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_views TO rt_excl.
  ENDMETHOD.                    "fill_fcode_excl

  METHOD fill_mode_save.
    rv_mode_save = 'A'.
  ENDMETHOD.                    "fill_mode_save

  METHOD set_grid_events.
    SET HANDLER handle_toolbar      FOR mo_grid.
    SET HANDLER handle_user_command FOR mo_grid.
    SET HANDLER handle_double_click FOR mo_grid.
  ENDMETHOD.                    "set_grid_events

  METHOD get_lvc_t_fcat_4_itab.
    DATA:
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lo_aggregations TYPE REF TO cl_salv_aggregations,
      lo_salv_table   TYPE REF TO cl_salv_table,
      ld_table        TYPE REF TO data.
    FIELD-SYMBOLS:
      <ft_table>         TYPE STANDARD TABLE.

    CREATE DATA ld_table LIKE it_table.
    ASSIGN ld_table->* TO <ft_table>.
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = lo_salv_table
          CHANGING
            t_table      = <ft_table> ).
      CATCH cx_salv_msg.
    ENDTRY.

    lo_columns  = lo_salv_table->get_columns( ).
    lo_aggregations = lo_salv_table->get_aggregations( ).

    rt_fcat =
      cl_salv_controller_metadata=>get_lvc_fieldcatalog(
        r_columns             = lo_columns
        r_aggregations        = lo_aggregations ).

  ENDMETHOD.                    "get_lvc_t_fcat_4_itab

ENDCLASS.                    "lcl_xxx_viewer IMPLEMENTATION
