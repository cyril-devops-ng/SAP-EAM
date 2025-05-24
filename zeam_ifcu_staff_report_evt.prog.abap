*&---------------------------------------------------------------------*
*& Include          ZEAM_IFCU_STAFF_REPORT_EVT
*&---------------------------------------------------------------------*
**********************************************************************
INITIALIZATION.
**********************************************************************
  t1 = 'Multiple Selection'.
  t2 = 'Single Selection'.
  tta = 'Report Selection'.

  logo1 = icon_equipment.
  title1 = 'Equipment:'.

  logo2 = icon_plant.
  title2 = 'Gas Station:'.

  logo3 = icon_date.
  title3 = 'Collection Date:'.

  logo4 = icon_transport.
  title4 = 'Collector ID:'.

  logo5 = icon_employee.
  title5 = 'Attendant ID:'.


  logo6 = icon_equipment.
  title6 = 'Equipment:'.

  logo7 = icon_plant.
  title7 = 'Gas Station:'.

  logo8 = icon_date.
  title8 = 'Collection Date:'.


**********************************************************************
AT SELECTION-SCREEN OUTPUT.
**********************************************************************
  IF p_s_read eq abap_true.
    configure_single_screen.
  ELSE.
    configure_multiple_screen.
  ENDIF.

**********************************************************************
END-OF-SELECTION.
**********************************************************************
call screen lcl_ifcu_staff_report=>gc_screen.
