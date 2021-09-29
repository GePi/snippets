CLASS lcl_xxx_viewer DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF cs_grid_user_func,
        " добавить
        addcontr TYPE ui_func VALUE 'ADDCONTR',
        " удалить
        delcontr TYPE ui_func VALUE 'DELCONTR',
      END OF cs_grid_user_func.
    METHODS:
      constructor IMPORTING io_dc_contr TYPE REF TO lcl_card_dc_contr,

      control_init,
      refresh,
      free,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

  PRIVATE SECTION.
    METHODS:
      fill_fcat RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat,
      fill_variant RETURNING VALUE(rs_var) TYPE disvariant,
      fill_layout RETURNING VALUE(rs_layout) TYPE lvc_s_layo,
      fill_fcode_excl RETURNING VALUE(rt_excl) TYPE ui_functions,
      fill_mode_save RETURNING VALUE(rv_mode_save) TYPE char01,

      set_grid_events,

      get_lvc_t_fcat_4_itab
        IMPORTING it_table       TYPE STANDARD TABLE
        RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.

    DATA:
      mo_dc_contr TYPE REF TO lcl_card_dc_contr.

    DATA mo_cont TYPE REF TO cl_gui_custom_container.
    DATA mo_grid TYPE REF TO cl_gui_alv_grid.

    DATA mv_cont_name TYPE fieldname VALUE 'CC_CONTRACTS'.
    DATA md_data_table TYPE REF TO data.

ENDCLASS. 

" РЕАЛИЗАЦИЯ
CLASS lcl_xxx_viewer IMPLEMENTATION.
  METHOD constructor.
    mo_dc_contr = io_dc_contr.
  ENDMETHOD.

  METHOD free.
    IF mo_grid IS NOT INITIAL.
      mo_grid->free( ).
    ENDIF.
    IF mo_cont IS NOT INITIAL.
      mo_cont->free( ).
    ENDIF.

    FREE mo_grid.
    FREE mo_cont.
  ENDMETHOD.

  METHOD refresh.
    IF mo_grid IS INITIAL.
      RETURN.
    ENDIF.
    mo_grid->refresh_table_display( ).
  ENDMETHOD.
  
  METHOD control_init.
    " Инициализация alv-grid для контрактов
    FIELD-SYMBOLS <ft_data> TYPE STANDARD TABLE.

    IF mo_grid IS NOT INITIAL.
      mo_grid->refresh_table_display( ).
      RETURN.
    ENDIF.

    CREATE OBJECT mo_cont
      EXPORTING
        container_name = mv_cont_name.

    CREATE OBJECT mo_grid
      EXPORTING
        i_parent = mo_cont.

    md_data_table = mo_dc_contr->get_ref_to_data( ).
    ASSIGN md_data_table->* TO <ft_data>.
    DATA(lt_fcat) = fill_fcat( ).
    DATA(ls_variant) = fill_variant( ).
    DATA(ls_layout) = fill_layout( ).
    DATA(lt_fcode_excl) = fill_fcode_excl( ).
    DATA(lv_mode_save)  = fill_mode_save( ).

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
  ENDMETHOD.

  METHOD fill_fcat.
    FIELD-SYMBOLS <ft_tab> TYPE STANDARD TABLE.
    ASSIGN md_data_table->* TO <ft_tab>.
    rt_fcat = get_lvc_t_fcat_4_itab( <ft_tab> ).

    LOOP AT rt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      CASE <fs_fcat>-fieldname .
        WHEN 'NUM'. " Номер договора
          <fs_fcat>-scrtext_s = text-z01.
          <fs_fcat>-scrtext_m = text-z02.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-z03.
        WHEN 'DAT'. " Дата
          <fs_fcat>-scrtext_s = text-z04.
          <fs_fcat>-scrtext_m = text-z05.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-z06.
        WHEN 'TYP_TXT'. " Тип
          <fs_fcat>-scrtext_s = text-z07.
          <fs_fcat>-scrtext_m = text-z08.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-z09.
        WHEN 'DESCR'. " Наименование
          <fs_fcat>-scrtext_s = text-z10.
          <fs_fcat>-scrtext_m = text-z11.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-z12.
        WHEN 'STAT_TXT'. " Статус
          <fs_fcat>-scrtext_s = text-z13.
          <fs_fcat>-scrtext_m = text-z14.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-z15.
        WHEN 'CREDITOR_TXT'. " Потребитель
          <fs_fcat>-scrtext_s = text-z16.
          <fs_fcat>-scrtext_m = text-z17.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-z18.
        WHEN 'DEBTOR_TXT'. " Поставщик
          <fs_fcat>-scrtext_s = text-z19.
          <fs_fcat>-scrtext_m = text-z20.
          <fs_fcat>-scrtext_l = <fs_fcat>-coltext = text-z21.

        WHEN OTHERS.
          <fs_fcat>-tech = abap_true.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_variant.
    rs_var-report = sy-repid.
    rs_var-username = sy-uname.
  ENDMETHOD.

  METHOD fill_layout.
    rs_layout-cwidth_opt = abap_true.
  ENDMETHOD.

  METHOD fill_fcode_excl.
    " исчезнуть кнопку с вариантами
    APPEND cl_gui_alv_grid=>mc_fc_maintain_variant TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_variant_admin TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_current_variant TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_info TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_find TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_find_more TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_print TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_subtot TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_sum TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_mb_sum TO rt_excl.

    APPEND cl_gui_alv_grid=>mc_fc_graph TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_detail TO rt_excl.
    APPEND cl_gui_alv_grid=>mc_fc_views TO rt_excl.
  ENDMETHOD.

  METHOD fill_mode_save.
    rv_mode_save = space.
  ENDMETHOD.

  METHOD set_grid_events.
    SET HANDLER handle_toolbar      FOR mo_grid.
    SET HANDLER handle_user_command FOR mo_grid.
  ENDMETHOD.

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
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.

    lo_columns  = lo_salv_table->get_columns( ).
    lo_aggregations = lo_salv_table->get_aggregations( ).

    rt_fcat =
      cl_salv_controller_metadata=>get_lvc_fieldcatalog(
        r_columns             = lo_columns
        r_aggregations        = lo_aggregations ).

  ENDMETHOD.

  METHOD handle_toolbar.
    INSERT:
      VALUE #( function = cs_grid_user_func-addcontr
               icon = icon_create
               quickinfo = 'Добавить договор'(t10)
               text = '' ) INTO e_object->mt_toolbar INDEX 1,
      VALUE #( function = cs_grid_user_func-delcontr
               icon = icon_delete
               quickinfo = 'Удалить договор'(t11)
               text = '' ) INTO e_object->mt_toolbar INDEX 2.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN cs_grid_user_func-addcontr.
      WHEN cs_grid_user_func-delcontr.
    ENDCASE.
  ENDMETHOD.
ENDCLASS. 
