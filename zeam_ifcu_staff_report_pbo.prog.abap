*----------------------------------------------------------------------*
***INCLUDE ZEAM_IFCU_STAFF_REPORT_PBO.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
 SET PF-STATUS 'ZSTATUS_0100'.
 SET TITLEBAR 'ZTITLE_100'.
 try.
  data(lo_report)
  =  lcl_ifcu_staff_report=>create_report(
     EXPORTING
       if_equnr        = p_equnr
       if_station      = p_stn
       if_posting_date = p_pdate
       ir_equnr        = s_eqnr[]
       ir_station_t    = s_stn[]
       ir_posting_date = s_pdate[]
       ir_collector_id = s_coll[]
       ir_attendant_id = s_att[]
   ).

  lo_report->create_grid(
    CHANGING
      ct_tab = gt_output
  ).

  lo_report->show_grid( ).
 catch zcx_eam_exception into data(lo_cx_eam).
   data(lt_msg) = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
   zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
   leave to screen 0.
 endtry.
ENDMODULE.
