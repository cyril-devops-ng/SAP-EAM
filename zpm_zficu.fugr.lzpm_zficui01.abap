MODULE pai_100 INPUT.
  " ---------------------------------------------------------------------
  " 10: Assign user-command globally
  " ---------------------------------------------------------------------
  gf_ucomm = sy-ucomm.
  " ---------------------------------------------------------------------
  " 20: Handle user-command
  " ---------------------------------------------------------------------
  CASE gf_ucomm.
      " ---------------------------------------------------------------------
      " 30: Exit command
      " ---------------------------------------------------------------------
    WHEN 'BACK' OR '%EX' OR 'RW' OR 'E'
      OR 'ENDE' OR 'ECAN' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
      " ---------------------------------------------------------------------
      " 40: Displ. Assignment
      " ---------------------------------------------------------------------
    WHEN lcl_pm_ifcu=>gc_display_assignment OR 'TCHK'.
      TRY.
          " ---------------------------------------------------------------------
          " 50: Create local object instance
          " ---------------------------------------------------------------------
          IF gf_staff_id IS NOT INITIAL.
            go_ifcu = NEW #( ir_staff_id    = VALUE #( ( low = gf_staff_id sign = 'I' option = 'EQ' ) )
                             if_ucomm       = gf_ucomm
                             if_temp_access = gf_show_t_access ).
          ELSE.
            go_ifcu = NEW #( if_ucomm       = gf_ucomm
                             if_temp_access = gf_show_t_access ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 60: Method call: GetStaffAccess
          " ---------------------------------------------------------------------
          go_ifcu->get_staff_access( ).
          go_ifcu->create_grid( CHANGING ct_tab = gt_staff_access ).
          " ---------------------------------------------------------------------
          " 70: Refresh grid
          " ---------------------------------------------------------------------
          go_ifcu->refresh_grid( ).
          " ---------------------------------------------------------------------
          " 80: Show grid
          " ---------------------------------------------------------------------
          go_ifcu->show_grid( ).
          " ---------------------------------------------------------------------
          " 90: Handle exceptions
          " ---------------------------------------------------------------------
        CATCH zcx_eam_exception INTO lo_cx_eam. " EAM Exception Class
          lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
          zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
      ENDTRY.
      " ---------------------------------------------------------------------
      " 100: Create Assignment command
      " ---------------------------------------------------------------------
    WHEN lcl_pm_ifcu=>gc_create_assignment.
      CLEAR lt_msg.
      " ---------------------------------------------------------------------
      " 110: Check Staff ID is provided
      " ---------------------------------------------------------------------
      IF gf_staff_id IS INITIAL.
        TRY.
            " ---------------------------------------------------------------------
            " 120: Raise exception
            " ---------------------------------------------------------------------
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_staffid_is_mandatory ).
            " ---------------------------------------------------------------------
            " 130: Catch exception
            " ---------------------------------------------------------------------
          CATCH zcx_eam_exception INTO lo_cx_eam.
            lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
            zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
            RETURN.
        ENDTRY.
      ENDIF.
      " ---------------------------------------------------------------------
      " 140.0.1 Select assignment option
      " ---------------------------------------------------------------------
      DATA lf_result TYPE char1.
      CALL FUNCTION 'K_KKB_POPUP_RADIO2'
        EXPORTING
          i_title   = 'Assignment Option'
          i_text1   = |{ icon_transportation_mode } Station Assignment|
          i_text2   = |{ icon_location } Location Assignment|
          i_default = '1'
        IMPORTING
          i_result  = lf_result
        EXCEPTIONS
          cancel    = 1
          OTHERS    = 2.
      CHECK xsdbool( sy-subrc = 1 ) EQ abap_false.
      CASE lf_result.
        WHEN '1'.
          " ---------------------------------------------------------------------
          " 140: Get stations
          " ---------------------------------------------------------------------
          gt_stations = zcl_eam_ifcu_gen=>display_station_selection( VALUE #( )  ).
        WHEN '2'.
          " ---------------------------------------------------------------------
          " 140: Get Locations
          " ---------------------------------------------------------------------
          gt_locations = zcl_eam_ifcu_gen=>display_location_selection( VALUE #( )  ).
          CHECK xsdbool( gt_locations IS INITIAL ) = abap_false.
          gt_stations = zcl_eam_ifcu_gen=>create_stations_from_locations( gt_locations  ).
      ENDCASE.

      " ---------------------------------------------------------------------
      " 150: Check that station is selected
      " ---------------------------------------------------------------------
      CHECK xsdbool( gt_stations IS INITIAL ) = abap_false.
      " ---------------------------------------------------------------------
      " 160: Loop: Selected.Stations
      " ---------------------------------------------------------------------
      " ---------------------------------------------------------------------
      " 160.1 Confirm
      " ---------------------------------------------------------------------
      DATA lf_confirm TYPE char1.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = `Staff Assignment`
          text_question         = COND #( WHEN lf_result = '1'
                                          THEN `Assign Staff to Station(s) (Y/N)?`
                                          ELSE `Assign Staff to Location(s) (Y/N)?` )
          text_button_1         = `Yes`
          text_button_2         = `No`
          icon_button_1         = 'ICON_CHECKED'
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lf_confirm.
      CHECK lf_confirm = 1.
      LOOP AT gt_stations ASSIGNING FIELD-SYMBOL(<fs_station>)
           WHERE flag = abap_true.
        TRY.
            " ---------------------------------------------------------------------
            " 170: Create local object instance
            " ---------------------------------------------------------------------
            go_ifcu = NEW #( ir_staff_id = VALUE
                             zcl_eam_ifcu=>tr_staff_id( ( low = gf_staff_id sign = 'I' option = 'EQ' ) )
                             ir_station  = VALUE zcl_eam_ifcu=>tr_station(
                                                     ( low = <fs_station>-station sign = 'I' option = 'EQ' ) )
                             if_ucomm    = gf_ucomm ).
            " ---------------------------------------------------------------------
            " 180: Method call: CreateAccess
            " ---------------------------------------------------------------------
            go_ifcu->create_access( ).
            " ---------------------------------------------------------------------
            " 190: Handle exceptions
            " ---------------------------------------------------------------------
          CATCH zcx_eam_exception INTO lo_cx_eam.
            DATA(l_msg) = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
            convert_message_to_success 'ZEAM_MSG' '007' l_msg.
            APPEND LINES OF l_msg TO lt_msg.
        ENDTRY.
        CLEAR go_ifcu.
        IF go_ifcu IS NOT BOUND.
          TRY.
              go_ifcu = NEW #( ir_staff_id = VALUE
                              zcl_eam_ifcu=>tr_staff_id( ( low = gf_staff_id sign = 'I' option = 'EQ' ) )
                              if_ucomm    = gf_ucomm ).
              go_ifcu->refresh_grid(  ).
            CATCH zcx_eam_exception.
          ENDTRY.
        ENDIF.
      ENDLOOP.
      " ---------------------------------------------------------------------
      " 200: Display messages
      " ---------------------------------------------------------------------
      IF lt_msg IS NOT INITIAL.
        zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
        IF go_ifcu IS NOT BOUND.
          TRY.
              go_ifcu = NEW #( ir_staff_id = VALUE
                              zcl_eam_ifcu=>tr_staff_id( ( low = gf_staff_id sign = 'I' option = 'EQ' ) )
                              if_ucomm    = gf_ucomm ).
              go_ifcu->refresh_grid(  ).
            CATCH zcx_eam_exception.
          ENDTRY.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMODULE.

MODULE get_staff_name INPUT.
  " ---------------------------------------------------------------------
  " 10: Get Staff name
  " ---------------------------------------------------------------------
  CLEAR gf_staff_name.
  SELECT SINGLE FROM pa0002
    FIELDS concat_with_space(
               initcap( pa0002~vorna ),
               initcap( pa0002~nachn ) ,
               1 )                       AS gf_staff_name
    WHERE pernr = @gf_staff_id
    INTO @gf_staff_name.
ENDMODULE.

MODULE temporary_access_check INPUT.
  " ---------------------------------------------------------------------
  " do nothing since usr-command handles already
  " ---------------------------------------------------------------------
ENDMODULE.
MODULE pai_200 INPUT.
  " ---------------------------------------------------------------------
  " 10: Assign user-command globally
  " ---------------------------------------------------------------------
  gf_ucomm = sy-ucomm.
  " ---------------------------------------------------------------------
  " 20: Handle user-command
  " ---------------------------------------------------------------------
  CASE gf_ucomm.
      " ---------------------------------------------------------------------
      " 30: Exit command
      " ---------------------------------------------------------------------
    WHEN 'BACK' OR '%EX' OR 'RW' OR 'E'
      OR 'ENDE' OR 'ECAN' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
      " ---------------------------------------------------------------------
      " 40: Handle create temp.access cmd
      " ---------------------------------------------------------------------
    WHEN lcl_pm_ifcu=>gc_create_temp_ass.
      CLEAR lt_msg.
      " ---------------------------------------------------------------------
      " 50: Check station is provided
      " ---------------------------------------------------------------------
      IF gt_stations IS INITIAL.
        TRY.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_station_is_mandatory ).
          CATCH zcx_eam_exception INTO DATA(lo_eam_ex).
            CLEAR lt_msg.
            lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_eam_ex ).
            zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
        ENDTRY.
        RETURN.
      ENDIF.
      LOOP AT gt_stations ASSIGNING <fs_station>
           WHERE flag = abap_true.
        TRY.
            " ---------------------------------------------------------------------
            " 60: Get access reason
            " ---------------------------------------------------------------------
            DATA lt_text TYPE TABLE OF tdline.
            go_t_editor->get_text_as_r3table( IMPORTING table = lt_text ).
            CLEAR gf_access_reason.
            LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
              gf_access_reason = |{ gf_access_reason } { <fs_text> }|.
            ENDLOOP.
            " ---------------------------------------------------------------------
            " 70: Create temp. assignment factory
            " ---------------------------------------------------------------------
            DATA(lo_eam_ifcu) = zcl_eam_ifcu=>create_temp_staff_assignment(
                                    station       = <fs_station>-station
                                    staff_id      = gf_staff_id
                                    valid_from    = gf_valid_from
                                    valid_to      = gf_valid_to
                                    ifcu_process  = zcl_eam_ifcu=>gc_new_temp_staff_station_acc
                                    access_reason = gf_access_reason ).
            " ---------------------------------------------------------------------
            " 80: Confirm user action
            " ---------------------------------------------------------------------
            CLEAR lf_confirm.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = `Temporary Staff Assignment`
                text_question         = `Assign Temporary Station Access to Staff (Y/N)?`
                text_button_1         = `Yes`
                text_button_2         = `No`
                icon_button_1         = 'ICON_CHECKED'
                icon_button_2         = 'ICON_CANCEL'
                display_cancel_button = abap_false
              IMPORTING
                answer                = lf_confirm.
            IF lf_confirm <> 1.
              CONTINUE.
            ENDIF.
            " ---------------------------------------------------------------------
            " 90: Create temporary access
            " ---------------------------------------------------------------------
            lo_eam_ifcu->create_stac( ).
            " ---------------------------------------------------------------------
            " 100: Catch exceptions
            " ---------------------------------------------------------------------
          CATCH zcx_eam_exception INTO DATA(lo_cx_exc).
            l_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_exc ).
            convert_message_to_success 'ZEAM_MSG' '008' l_msg.
            APPEND LINES OF l_msg TO lt_msg.
        ENDTRY.
      ENDLOOP.
      IF lt_msg IS NOT INITIAL.
        zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
        IF line_exists( lt_msg[ id     = 'ZEAM_MSG'
                                number = '008'
                                type   = 'S' ] ).
          CLEAR: gf_valid_from,
                 gf_valid_to,
                 gf_staff_id,
                 gt_stations,
                 gf_staff_name.
          CLEAR lt_text.
          go_t_editor->set_text_as_r3table( lt_text ).
        ENDIF.
      ENDIF.
      " ---------------------------------------------------------------------
      " 110: Handle station selection cmd
      " ---------------------------------------------------------------------
    WHEN lcl_pm_ifcu=>gc_select_stations.
      gt_stations = zcl_eam_ifcu_gen=>display_station_selection( VALUE #( ( ) ) ).
  ENDCASE.
ENDMODULE.
