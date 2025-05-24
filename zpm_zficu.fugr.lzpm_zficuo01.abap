*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  " ---------------------------------------------------------------------
  " 10: Set pf-status
  " ---------------------------------------------------------------------
  SET PF-STATUS 'Z100_STAT'.
  " ---------------------------------------------------------------------
  " 20: Set title bar
  " ---------------------------------------------------------------------
  SET TITLEBAR 'Z100_TTL'.
  " ---------------------------------------------------------------------
  " 30: If No local object
  " ---------------------------------------------------------------------
  " go_ifcu IS INITIAL and
  IF gf_ucomm IS INITIAL.
    TRY.
        " ---------------------------------------------------------------------
        " 40: Create Local object
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
        " 50: Method call: GetStaffAccess
        " ---------------------------------------------------------------------
        go_ifcu->get_staff_access( ).
        " ---------------------------------------------------------------------
        " 60: Create Grid
        " ---------------------------------------------------------------------
        go_ifcu->create_grid( CHANGING ct_tab = gt_staff_access ).
        " ---------------------------------------------------------------------
        " 70: Show Grid
        " ---------------------------------------------------------------------
        go_ifcu->show_grid( ).
        " ---------------------------------------------------------------------
        " 80: Handle exception
        " ---------------------------------------------------------------------
      CATCH zcx_eam_exception INTO DATA(lo_cx_eam). " EAM Exception Class
        DATA(lt_msg) = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
        zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
    ENDTRY.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  " ---------------------------------------------------------------------
  " 10: Set pf-status
  " ---------------------------------------------------------------------
  SET PF-STATUS 'Z200_STAT'.
  " ---------------------------------------------------------------------
  " 20: Set title bar
  " ---------------------------------------------------------------------
  SET TITLEBAR 'Z200_TTL'.
**********************************************************************
*30: Create editor
**********************************************************************
  IF go_editor IS INITIAL.
    go_editor = NEW #( `GO_EDITOR` ).

    go_t_editor = NEW #(
        parent           = go_editor
        max_number_chars = 255
        name             = `GF_ACCESS_REASON`
        wordwrap_mode    = 2
        wordwrap_position = 47
        wordwrap_to_linebreak_mode = 1

    ).
    go_t_editor->set_status_text( 'Enter the reason for temporary access' ).
    go_t_editor->set_toolbar_mode( 0 ).
    go_t_editor->set_statusbar_mode( 0 ).

  ENDIF.
ENDMODULE.
