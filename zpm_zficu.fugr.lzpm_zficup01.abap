CLASS lcl_pm_ifcu IMPLEMENTATION.
  METHOD constructor.
    " ---------------------------------------------------------------------
    " 10: Assign attr. Staff ID if provided.
    " ---------------------------------------------------------------------
    IF ir_staff_id IS SUPPLIED.
      gr_staff_id = ir_staff_id.
    ENDIF.
    " ---------------------------------------------------------------------
    " 20: Assign attr. Station if provided
    " ---------------------------------------------------------------------
    IF ir_station IS SUPPLIED.
      gr_station = ir_station.
    ENDIF.
    " ---------------------------------------------------------------------
    " 30: Assign Temp. access if provided
    " ---------------------------------------------------------------------
    IF if_temp_access IS SUPPLIED.
      gf_temp_access = if_temp_access.
    ENDIF.
    " ---------------------------------------------------------------------
    " 40: Assign User-command
    " ---------------------------------------------------------------------
    me->gf_ucomm = if_ucomm.
    " ---------------------------------------------------------------------
    " 50: Create Global Class object
    " ---------------------------------------------------------------------
    go_eam_ifcu = NEW #( ).
  ENDMETHOD.

  METHOD map_ucomm_to_ifcu_process.
    " ---------------------------------------------------------------------
    " 10: Assign user-command
    " ---------------------------------------------------------------------
    gf_ucomm = if_ucomm.
    " ---------------------------------------------------------------------
    " 20: Check user-command
    " ---------------------------------------------------------------------
    CASE gf_ucomm.
      " ---------------------------------------------------------------------
      " 30: Map process: create assignment
      " ---------------------------------------------------------------------
      WHEN gc_create_assignment.
        rf_ifcu_process = zcl_eam_ifcu=>gc_new_staff_station_access.
        " ---------------------------------------------------------------------
        " 40: Map process: display assignment
        " ---------------------------------------------------------------------
      WHEN gc_display_assignment.
        rf_ifcu_process = zcl_eam_ifcu=>gc_get_staff_access.
        " ---------------------------------------------------------------------
        " 50: Map process: revoke assignment
        " ---------------------------------------------------------------------
      WHEN gc_revoke_assignment.
        rf_ifcu_process = zcl_eam_ifcu=>gc_revoke_staff_station_access.
    ENDCASE.
  ENDMETHOD.

  METHOD create_grid.
    " ---------------------------------------------------------------------
    " 10: Create container
    " ---------------------------------------------------------------------
    IF xsdbool( go_access IS INITIAL )
       = abap_true.
      go_access = NEW #( 'GO_ACCESS' ).
      " ---------------------------------------------------------------------
      " 20: Create Alv
      " ---------------------------------------------------------------------
      TRY.
          IF go_alv IS INITIAL.
            cl_salv_table=>factory( EXPORTING r_container  = go_access
                                    IMPORTING r_salv_table = go_alv
                                    CHANGING  t_table      = gt_staff_access ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 30: Handle exception
          " ---------------------------------------------------------------------
        CATCH cx_salv_msg INTO DATA(lo_alv_ex).
          RAISE EXCEPTION NEW zcx_eam_exception( textid   = zcx_eam_exception=>eam_alv_error
                                                 previous = lo_alv_ex ).
      ENDTRY.
    ENDIF.
    " ---------------------------------------------------------------------
    " 40: Check Alv is created
    " ---------------------------------------------------------------------
    IF go_alv IS BOUND.
      TRY.
          " ---------------------------------------------------------------------
          " 50: Add toolbar functions
          " ---------------------------------------------------------------------
          add_func 'Remove Assignment' 'REVOKE_ACCESS' '@VZ@' 'Withdraw Assignment'.
          add_func 'Refresh' 'REFRESH' '@42@' 'Refresh'.
          go_alv->get_functions( )->set_all( abap_true ).
          " ---------------------------------------------------------------------
          " 60: Add event handlers
          " ---------------------------------------------------------------------
          SET HANDLER on_added_function FOR go_alv->get_event( ).
          SET HANDLER on_link_click     FOR go_alv->get_event( ).
        CATCH cx_salv_existing
              cx_salv_wrong_call
              cx_salv_method_not_supported.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD get_staff_access.
    " ---------------------------------------------------------------------
    " 10: Create object
    " ---------------------------------------------------------------------
    IF xsdbool( go_eam_ifcu IS NOT BOUND
    OR go_eam_ifcu IS INITIAL )
       = abap_true.
      go_eam_ifcu = NEW #( ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 20: Get staff access
    " ---------------------------------------------------------------------
    gt_staff_access =
    go_eam_ifcu->get_staff_access( ir_staff_id            = gr_staff_id
                                   ir_station             = gr_station
                                   if_include_temp_access = gf_temp_access ).
    FREE go_eam_ifcu.
  ENDMETHOD.

  METHOD on_added_function.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_selected_rows TYPE TABLE OF zcl_eam_ifcu=>t_station_access.

    " ---------------------------------------------------------------------
    " 20: Retrieve Selected Row Indexes
    " ---------------------------------------------------------------------
    DATA(lt_sel_rows) = go_select->get_selected_rows( ).
    " ---------------------------------------------------------------------
    " 30: Retrieve Selected Row Records
    " ---------------------------------------------------------------------
    LOOP AT lt_sel_rows
         ASSIGNING FIELD-SYMBOL(<fs_index_rows>).
      IF line_exists( gt_staff_access[ <fs_index_rows> ] ).
        APPEND gt_staff_access[ <fs_index_rows> ]
               TO lt_selected_rows.
      ENDIF.
    ENDLOOP.
    " ---------------------------------------------------------------------
    " 40: Handle tool bar func.
    " ---------------------------------------------------------------------
    CASE e_salv_function.
      " ---------------------------------------------------------------------
      " 50: Refresh func.
      " ---------------------------------------------------------------------
      WHEN 'REFRESH'.
        TRY.
            " ---------------------------------------------------------------------
            " 60: Refresh grid
            " ---------------------------------------------------------------------
            refresh_grid( ).
            " ---------------------------------------------------------------------
            " 70: Handle exception
            " ---------------------------------------------------------------------
          CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
            DATA(lt_msg) = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
            zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
        ENDTRY.
        " ---------------------------------------------------------------------
        " 80: Revoke access func.
        " ---------------------------------------------------------------------
      WHEN 'REVOKE_ACCESS'.
        " ---------------------------------------------------------------------
        " 90: Check if selected
        " ---------------------------------------------------------------------
        IF lt_selected_rows IS INITIAL.
          TRY.
              " ---------------------------------------------------------------------
              " 100: Raise exception
              " ---------------------------------------------------------------------
              RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_no_record_selected ).
            CATCH zcx_eam_exception INTO lo_cx_eam.
              lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
              zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
          ENDTRY.
        ELSE.
          " ---------------------------------------------------------------------
          " 110: Revoke access
          " ---------------------------------------------------------------------
          TRY.
              me->gf_ucomm = gc_revoke_assignment.
              revoke_access( lt_selected_rows ).
              " ---------------------------------------------------------------------
              " 90: Exception handling
              " ---------------------------------------------------------------------
            CATCH zcx_eam_exception INTO lo_cx_eam.
*              lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
*              zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
          ENDTRY.
          " ---------------------------------------------------------------------
          " 100: Refresh grid.
          " ---------------------------------------------------------------------
          CLEAR lt_msg.
          TRY.
              refresh_grid( ).
            CATCH zcx_eam_exception INTO lo_cx_eam.
              lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
              zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
          ENDTRY.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD on_link_click.
  ENDMETHOD.

  METHOD refresh_grid.
    " ---------------------------------------------------------------------
    " 10: Unset staff access
    " ---------------------------------------------------------------------
    CLEAR gt_staff_access.
    TRY.
        " ---------------------------------------------------------------------
        " 20: Get StaffAccess
        " ---------------------------------------------------------------------
        get_staff_access( ).
        " ---------------------------------------------------------------------
        " 30: Raise exception
        " ---------------------------------------------------------------------
      CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
        RAISE EXCEPTION lo_cx_eam.
    ENDTRY.
    " ---------------------------------------------------------------------
    " 40: Alv. refresh
    " ---------------------------------------------------------------------
    go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD revoke_access.
    " ---------------------------------------------------------------------
    " 10: Unset messages.
    " ---------------------------------------------------------------------
    CLEAR lt_msg.
    " ---------------------------------------------------------------------
    " 10.1 Confirm
    " ---------------------------------------------------------------------
    DATA lf_confirm TYPE char1.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING titlebar              = `Withdraw Staff Assignment`
                text_question         = `Withdraw Staff Assignment(s) (Y/N)?`
                text_button_1         = `Yes`
                text_button_2         = `No`
                icon_button_1         = 'ICON_CHECKED'
                icon_button_2         = 'ICON_CANCEL'
                display_cancel_button = abap_false
      IMPORTING answer                = lf_confirm.
    IF lf_confirm <> 1.
      RETURN.
    ENDIF.
    LOOP AT it_selection ASSIGNING FIELD-SYMBOL(<fs_selection>).
      " ---------------------------------------------------------------------
      " 20: Create Global object using Factory method
      " ---------------------------------------------------------------------
      TRY.
          IF <fs_selection>-temporary_access = abap_true.
            DATA(lo_eam_ifcu) =
              zcl_eam_ifcu=>create_temp_staff_assignment( station        = <fs_selection>-station
                                                          staff_id       = <fs_selection>-staff_id
                                                          valid_from     = <fs_selection>-valid_from
                                                          valid_to       = <fs_selection>-valid_to
                                                          access_reason  = ``
                                                          ifcu_process   = map_ucomm_to_ifcu_process( me->gf_ucomm )
                                                          if_temp_access = <fs_selection>-temporary_access ).
          ELSE.
            lo_eam_ifcu =
            zcl_eam_ifcu=>create_staff_assignment( station        = <fs_selection>-station
                                                   staff_id       = <fs_selection>-staff_id
                                                   if_temp_access = <fs_selection>-temporary_access
                                                   ifcu_process   = map_ucomm_to_ifcu_process( me->gf_ucomm ) ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 30: Revoke SACC
          " ---------------------------------------------------------------------
          lo_eam_ifcu->revoke_sacc( ).
          " ---------------------------------------------------------------------
          " 40: Catch exceptions
          " ---------------------------------------------------------------------
        CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
          refresh_grid( ).
          DATA(l_msg) = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
          convert_message_to_success 'ZEAM_MSG' '031' l_msg.
          APPEND LINES OF l_msg TO lt_msg.
      ENDTRY.
    ENDLOOP.
    " ---------------------------------------------------------------------
    " 50: Handle exception
    " ---------------------------------------------------------------------
    IF lt_msg IS NOT INITIAL.
      zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
    ENDIF.
  ENDMETHOD.

  METHOD show_grid.
    TRY.
        " ---------------------------------------------------------------------
        " 10: Define structure
        " ---------------------------------------------------------------------
        DATA lo_str TYPE REF TO cl_abap_structdescr.
        lo_str ?= cl_abap_structdescr=>describe_by_data( VALUE zcl_eam_ifcu=>t_station_access( )  ).
        go_columns = go_alv->get_columns( ).
        " ---------------------------------------------------------------------
        " 20: Set field catalog
        " ---------------------------------------------------------------------
        DO lines( lo_str->components ) TIMES.
          ASSIGN lo_str->components[ sy-index ] TO FIELD-SYMBOL(<fs_component>).
          CASE <fs_component>-name.
            WHEN 'STAFF_ID'.
              _thiscolumn.
              go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
            WHEN 'TEMPORARY_ACCESS'.
              _thiscolumn.
              go_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
              go_column->set_icon( 'X' ).
              go_column->set_output_length( 10 ).
              go_column->set_short_text( `Temporary` ).
              go_column->set_medium_text( `Temporary` ).
              IF gf_show_t_access = abap_false.
                go_column->set_visible( abap_false ).
              ELSE.
                _thiscolumn.
                go_column->set_visible( abap_true ).
              ENDIF.
            WHEN 'FIRST_NAME' OR 'LAST_NAME'.
              _thiscolumn.
              go_column->set_output_length( 20 ).
            WHEN 'LOC_NAME'.
               _thiscolumn.
              go_column->set_output_length( 25 ).
            WHEN 'STATION_NAME'.
              _thiscolumn.
              go_column->set_short_text( `Station` ).
              go_column->set_long_text( `Station Name` ).
              go_column->set_output_length( 20 ).
            WHEN 'STATION' OR 'STATION_T'.
              _thiscolumn.
              go_column->set_output_length( 10 ).
            WHEN 'VALID_FROM' OR 'VALID_TO'.
              IF gf_show_t_access = abap_false.
                _thiscolumn.
                go_column->set_visible( abap_false ).
              ELSE.
                _thiscolumn.
                go_column->set_visible( abap_true ).
              ENDIF.
          ENDCASE.
        ENDDO.
        " ---------------------------------------------------------------------
        " 30: Selection
        " ---------------------------------------------------------------------
        go_select = go_alv->get_selections( ).
*        go_alv->get_columns( )->set_optimize( abap_true ).
        IF xsdbool(  go_select IS NOT INITIAL ) = abap_true.
          go_select->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        ENDIF.
        go_alv->display( ).
        " ---------------------------------------------------------------------
        " 40: Exception
        " ---------------------------------------------------------------------
      CATCH cx_salv_not_found INTO DATA(lo_alv_ex).
        RAISE EXCEPTION NEW zcx_eam_exception( textid   = zcx_eam_exception=>eam_alv_error
                                               previous = lo_alv_ex ).
    ENDTRY.
  ENDMETHOD.

  METHOD create_access.
    " ---------------------------------------------------------------------
    " 10: Check station, staff is provided
    " ---------------------------------------------------------------------
    TRY.
        IF xsdbool( me->gr_station IS NOT INITIAL
                    AND me->gr_staff_id IS NOT INITIAL )
           = abap_true.
          " ---------------------------------------------------------------------
          " 20: Create Global object using Factory method
          " ---------------------------------------------------------------------
          DATA(lo_eam_ifcu) =
          zcl_eam_ifcu=>create_staff_assignment( station      = me->gr_station[ 1 ]-low
                                                 staff_id     = me->gr_staff_id[ 1 ]-low
                                                 ifcu_process = map_ucomm_to_ifcu_process( me->gf_ucomm ) ).
          " ---------------------------------------------------------------------
          " 30: Create SACC
          " ---------------------------------------------------------------------
          lo_eam_ifcu->create_sacc( ).
        ELSE.
          " ---------------------------------------------------------------------
          " 40: Raise exception
          " ---------------------------------------------------------------------
          RAISE EXCEPTION NEW zcx_eam_exception( textid = COND #( WHEN gr_staff_id IS INITIAL THEN
                                                                    zcx_eam_exception=>eam_staffid_is_mandatory
                                                                  WHEN gr_station IS INITIAL THEN
                                                                    zcx_eam_exception=>eam_station_is_mandatory ) ).

        ENDIF.
        " ---------------------------------------------------------------------
        " 50: Handle exception
        " ---------------------------------------------------------------------
      CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
*        refresh_grid( ).
        RAISE EXCEPTION lo_cx_eam.
    ENDTRY.
  ENDMETHOD.

  METHOD create_temporary_access.
  ENDMETHOD.
ENDCLASS.
