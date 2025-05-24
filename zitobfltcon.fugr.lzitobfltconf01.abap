*----------------------------------------------------------------------*
***INCLUDE LITOBFLTCONF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  LEAVE_TO_CALLER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM leave_to_caller.

  IF sy-calld IS INITIAL.
    LEAVE TO SCREEN 0.
  ELSE.
    LEAVE.
  ENDIF.

ENDFORM.                               " LEAVE_TO_CALLER

*&---------------------------------------------------------------------*
*&      Form  LEAVE_TO_START
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM leave_to_start.

  IF sy-calld IS INITIAL.
    SET SCREEN 0.
    LEAVE SCREEN.
  ELSE.
    LEAVE.
  ENDIF.

ENDFORM.                               " LEAVE_TO_START

*&---------------------------------------------------------------------*
*&      Form  START_MAIN_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_main_screen.

  g_check_req = c_x.
  SET SCREEN 200.

ENDFORM.                               " START_MAIN_SCREEN

*&---------------------------------------------------------------------*
*&      Form  INIT_EQUI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_equi.
*-> current time
  g_post_date = sy-datum.                                   " P1EK020652
  g_post_time = sy-uzeit.                                   " P1EK020652
*-> read relevant mpoints
  CALL FUNCTION 'ITOB_CONSUMPTION_MPPOS_GET'
    EXPORTING
      i_equnr            = itob-equnr
    IMPORTING
      e_itob             = itob
      e_fleet            = fleet
    TABLES
      t_mpoints          = gt_mpoints
      t_consp            = gt_consp
      t_count            = gt_count
    EXCEPTIONS
      not_found          = 1
      no_fleet_object    = 2
      no_measurem_points = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*-> fill rest of CONSUMPTION
  LOOP AT gt_consp.
    CALL FUNCTION 'CONVERSION_EXIT_LUNIT_OUTPUT'
      EXPORTING
        input          = gt_consp-msehi
      IMPORTING
        output         = gt_consp-mseh6
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
*-> det. fluid type
    IF NOT g_station IS INITIAL.
      PERFORM det_fluid_type USING    fleet
                             CHANGING gt_consp.
    ENDIF.
    MODIFY gt_consp.
  ENDLOOP.
*-> fill rest of COUNTER
  LOOP AT gt_count.
    CALL FUNCTION 'CONVERSION_EXIT_LUNIT_OUTPUT'
      EXPORTING
        input          = gt_count-msehi
      IMPORTING
        output         = gt_count-mseh6
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    MODIFY gt_count.
  ENDLOOP.
*-> ... and last counter
  PERFORM read_last_counter.

ENDFORM.                               " INIT_EQUI

*&---------------------------------------------------------------------*
*&      Form  CHECK_COUNTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_counter.

  STATICS: s_msgno LIKE sy-msgno,
           s_msgv1 LIKE sy-msgv1,
           s_msgv2 LIKE sy-msgv2,
           s_msgv3 LIKE sy-msgv3,
           s_msgv4 LIKE sy-msgv4.

  DATA: l_ignore_gmove_err,
        l_no_t370con_tol_check,
        l_pri_calc LIKE fleet-pri_calc.

*--- Counter-check is not necessary for some funktions that have nothing
*--- to do with new counter-values
  CHECK NOT ( g_ok_code = 'LEAV' OR g_ok_code = 'CANC' OR
              g_ok_code = 'BACK' OR g_ok_code = 'IE03' OR
              g_ok_code = 'IW21' OR g_ok_code = 'IK22' OR
              g_ok_code = 'OFIO' OR g_ok_code = 'CAL1' OR
              g_ok_code = 'ODCR' ).

  IF g_check_req = c_x.
    g_dirty = c_x.
    l_pri_calc = fleet-pri_calc.
*-> if station given - Goods Movement is MUST
    IF g_station IS INITIAL.
      l_ignore_gmove_err = c_x.
    ENDIF.
*-> calc. rule reread (is buffered as custom. table)
    SELECT SINGLE * FROM t370clc INTO t370clc
           WHERE  calc_key = fleet-pri_calc.
    IF sy-subrc <> 0.
      l_no_t370con_tol_check = c_x.
    ENDIF.
    CALL FUNCTION 'ITOB_CONSUMPTION_UPDATE'
      EXPORTING
        i_equnr                 = itob-equnr
        i_date                  = g_post_date
        i_time                  = g_post_time
        i_station               = t370fld_stn_t-station
        i_ignore_gmove_err      = l_ignore_gmove_err
        i_no_t370con_tol_check  = l_no_t370con_tol_check
        i_check_only            = c_x
      IMPORTING
        e_err_point             = g_err_point
      TABLES
        t_consp                 = gt_consp
        t_count                 = gt_count
      EXCEPTIONS
        fleet_data_not_complete = 1
        no_station              = 2
        no_t370fld_gmove_entry  = 3
        gmove_error             = 4
        values_missing          = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      CLEAR g_ok_code.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSEIF NOT g_err_point IS INITIAL.
*-> ignore some messages in case of SAVE
      IF g_ok_code = 'SAVE' AND
         sy-msgid = 'ITOBFLTCON' AND
         ( sy-msgno = '030' OR
           sy-msgno = '032' OR
           sy-msgno = '040' OR
           sy-msgno = '041' OR
           sy-msgno = '042' OR
           sy-msgno = '043' OR
           sy-msgno = '044' ) AND
         s_msgno = sy-msgno AND
         s_msgv1 = sy-msgv1 AND
         s_msgv2 = sy-msgv2 AND
         s_msgv3 = sy-msgv3 AND
         s_msgv4 = sy-msgv4.
        CLEAR g_check_req.
      ELSE.
*-> save last ITOBFLTCON message to find whether it has
*   been displayed already
        IF sy-msgid = 'ITOBFLTCON'.
          s_msgno = sy-msgno.
          s_msgv1 = sy-msgv1.
          s_msgv2 = sy-msgv2.
          s_msgv3 = sy-msgv3.
          s_msgv4 = sy-msgv4.
        ELSE.
          CLEAR: s_msgno,
                 s_msgv1,
                 s_msgv2,
                 s_msgv3,
                 s_msgv4.
        ENDIF.
        CLEAR g_ok_code.
        MESSAGE ID sy-msgid TYPE c_msgty_error NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      CLEAR g_check_req.
    ENDIF.
*--- restore calculation rule
    fleet-pri_calc = l_pri_calc.
  ENDIF.
**********************************************************************
*Mike's enhancement
**********************************************************************
  DATA: lt_imrg TYPE TABLE OF imrg,
        ls_imrg TYPE imrg,
        lv_days TYPE i,
        recnt   TYPE imrc_cntrr, "counter reading
        lcntc   TYPE imrc_cntrr, "last counter reading
        cdiff   TYPE imrc_cdiff. "cdiff = recnt - lcntc.
  data lv_decimal TYPE p DECIMALS 2.

  " LOOP AT imrg_ins ASSIGNING FIELD-SYMBOL(imrg_ins).
  "validate that the
  "REFRESH: lt_imrg.
  "BREAK e8441.
  TRY.
  SELECT * FROM imrg INTO TABLE lt_imrg
    WHERE point = rifltcoun-point AND cancl <> 'X'.
  IF sy-subrc = 0.
    "sort lt_imrg by idate descending
    SORT lt_imrg BY mdocm DESCENDING.
    READ TABLE lt_imrg INTO ls_imrg INDEX 1.
    IF sy-subrc = 0.
      REPLACE ',' WITH '.' INTO rifltcoun-recnt.
      REPLACE ',' WITH '.' INTO rifltcoun-lcntc.
      recnt = rifltcoun-recnt.
      lcntc = rifltcoun-lcntc.
      cdiff = recnt - lcntc.
      "compare the last data and date entered
      IF g_post_date LE ls_imrg-idate.
        g_check_req = c_x.
        raise exception type zcx_eam_exception
        exporting
        textid = zcx_eam_exception=>imrg_rec_date_error
        imrg_idate = |{ ls_imrg-idate date = user }|.
*         MESSAGE I001(zpm_imrg) WITH ls_imrg-idate.
*         return.

      ENDIF.
      "compare last reading entry cannot be negative or less
      IF recnt LE ls_imrg-recdv.
        g_check_req = c_x.
        lv_decimal = ls_imrg-recdv.
        raise exception type zcx_eam_exception
        exporting
        textid = zcx_eam_exception=>imrg_rec_value_error
        imrg_value = conv #( lv_decimal )
        imrg_meins = rifltcoun-msehi.
*        MESSAGE I002(zpm_imrg) WITH lv_decimal rifltcoun-msehi.
*        return.
      ENDIF.
      "calculate maximum number of hours
      "giving the measurement unit, determine maximum value per unit of recording
      CASE rifltcoun-msehi.
        WHEN 'H' OR 'STD'. "apply time logic
          DATA(max_hours) = ( g_post_date - ls_imrg-idate ) * 8. "8 hours
          IF cdiff GT max_hours.
            g_check_req = c_x.
            raise exception type zcx_eam_exception
            exporting
            textid = zcx_eam_exception=>imrg_rec_value_high
            imrg_hours = conv #( max_hours )
            imrg_meins_s = 'Hrs'.
*            MESSAGE I003(zpm_imrg) WITH max_hours 'Hrs'.
*            return.
          ENDIF.
        WHEN 'KM'. "apply distance logic
          "validate that reading is greater than previous with at least 1KM
          IF cdiff LE 1.
            g_check_req = c_x.
            raise exception type zcx_eam_exception
            exporting
            textid = zcx_eam_exception=>imrg_rec_value_low.
*             MESSAGE I004(zpm_imrg).
*             return.
          ENDIF.
          DATA(no_of_days) = g_post_date - ls_imrg-idate.
          "get maximum anuual reading
          SELECT SINGLE pyear FROM imptt WHERE point = @rifltcoun-point INTO @DATA(an_reading).
          IF sy-subrc = 0 AND an_reading IS NOT INITIAL.
            "check with provided formular
            "previous reading + ((annual estimate/365) * number of days from previous reading)
            DIVIDE an_reading by 1000.
            DATA(max_reading) = ( an_reading / 365 ) * no_of_days.
            "DATA(read_total) = max_reading + ls_imrg-cntrr.
            "max reading should not be more than 150 KM daily
*            IF recnt GT read_total.
            "IF recnt GT max_reading.
            IF cdiff GT max_reading.
              lv_decimal = max_reading.
              g_check_req = c_x.
              raise exception type zcx_eam_exception
              exporting
              textid = zcx_eam_exception=>imrg_rec_error_005
              imrg_value = conv #( lv_decimal )
              imrg_days = conv #( no_of_days ).
*               MESSAGE I005(zpm_imrg) WITH lv_decimal no_of_days.
*               return.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          "not addressed, will be updated where necessary
      ENDCASE.

    ENDIF.
  ENDIF.
  CATCH ZCX_EAM_EXCEPTION into data(lo_cx_eam).
    g_check_req = c_x.
    data(lt_msg) = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
    zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
  endtry.
**********************************************************************
*End of Mike's enhancement
**********************************************************************
ENDFORM.                               " CHECK_COUNTER

*&---------------------------------------------------------------------*
*&      Form  UPDATE_COUNTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_counter.

  DATA: l_ignore_gmove_err,
        l_no_t370con_tol_check.

*-> update only when pre-check OK
  IF g_check_req IS INITIAL AND
     g_dirty = c_x.
*-> if station given - Goods Movement is MUST
    IF g_station IS INITIAL.
      l_ignore_gmove_err = c_x.
    ENDIF.
*-> calc. rule reread (is buffered as custom. table)
    SELECT SINGLE * FROM t370clc INTO t370clc
           WHERE  calc_key = fleet-pri_calc.
    IF sy-subrc <> 0.
      l_no_t370con_tol_check = c_x.
    ENDIF.
*-> write documents
    CALL FUNCTION 'ZITOB_CONSUMPTION_UPDATE'
      EXPORTING
        i_equnr                 = itob-equnr
        i_date                  = g_post_date
        i_time                  = g_post_time
        i_station               = t370fld_stn_t-station
        i_ignore_gmove_err      = l_ignore_gmove_err
        i_no_t370con_tol_check  = l_no_t370con_tol_check
      IMPORTING
        e_err_point             = g_err_point
      TABLES
        t_consp                 = gt_consp
        t_count                 = gt_count
      EXCEPTIONS
        fleet_data_not_complete = 1
        no_station              = 2
        no_t370fld_gmove_entry  = 3
        gmove_error             = 4
        values_missing          = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      CLEAR g_ok_code.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      g_check_req = c_x.
    ELSEIF NOT g_err_point IS INITIAL.
*-> don't SAVE any of the documents
      ROLLBACK WORK.
      CLEAR g_ok_code.
      MESSAGE ID sy-msgid TYPE c_msgty_error NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      g_check_req = c_x.
    ELSE.
*      BREAK csayeh.
      TRY.
          CLEAR lt_msg.
          IF gf_file IS NOT INITIAL.
            DATA(lo_c_ifcu) =
                zcl_eam_ifcu=>create_ifcu_consumption(
                  EXPORTING
                    equnr          = itob-equnr
                    station        = t370fld_stn_t-station
                    posting_date   = g_post_date
                    posting_time   = g_post_time
                    staff_id       = gf_attendant_id
                    short_text     = gf_remarks
                    attachment     = gf_file
                    attachment_ttl = CONV #( gf_doc_title )
                    ifcu_process   = zcl_eam_ifcu=>gc_new_fuel_consumption
                ).
          ELSE.
            lo_c_ifcu = zcl_eam_ifcu=>create_ifcu_consumption(
                EXPORTING
                  equnr          = itob-equnr
                  station        = t370fld_stn_t-station
                  posting_date   = g_post_date
                  posting_time   = g_post_time
                  staff_id       = gf_attendant_id
                  short_text     = gf_remarks
                  ifcu_process   = zcl_eam_ifcu=>gc_new_fuel_consumption
              ).
          ENDIF.
          lo_c_ifcu->create_ifcu( gf_staff_id ).
        CATCH zcx_eam_exception INTO DATA(lo_eam_cx).
          lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_eam_cx ).

          IF line_exists( lt_msg[ number = '033' ] ).
            assign lt_msg[ number = '033' ] to FIELD-SYMBOL(<fs_s_msg>).
            <fs_s_msg>-type = 'S'.
          endif.

          IF xsdbool( line_exists( lt_msg[ type = 'E' ] )
                   OR line_exists( lt_msg[ type = 'A' ] )
                   OR line_exists( lt_msg[ type = 'X' ] ) )
                   EQ abap_true.
            zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
            RETURN.
          ENDIF.
      ENDTRY.
      CLEAR:
        g_check_req,
        g_dirty.
      COMMIT WORK.
      MESSAGE i000 INTO DATA(lf_success).
      APPEND VALUE bapiret2( id = sy-msgid
                             type = 'S'
                             number = sy-msgno
                             message = 'Documents were produced successfully'
                              ) TO lt_msg.
*      MESSAGE i000.                                            "n2661823
      zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
      PERFORM leave_to_start.
    ENDIF.
  ENDIF.
*-> clear buffer in case that SAVE event was performed without success
  CALL FUNCTION 'MEASUREM_BUFFER_INITIALIZE'.

ENDFORM.                               " UPDATE_COUNTER

*&---------------------------------------------------------------------*
*&      Form  CONSP_PAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consp_pai.
  DATA: l_index LIKE sy-stepl.
  CLEAR: lt_msg.
  TRY.
      DATA(lf_ft_validation) =
      zcl_eam_ifcu_gen=>validate_fuel_type(
        EXPORTING
          if_equnr     = itob-equnr
          if_fuel_type = rifltcons-fluid_type
      ).
    CATCH zcx_eam_exception INTO DATA(lo_eam_exc).
      lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_eam_exc ).
      zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
  ENDTRY.

  CHECK lf_ft_validation EQ abap_false.
  CLEAR: lf_ft_validation.

  IF rifltcons-recdf <> gt_consp-recdf
  OR rifltcons-fluid_type <> gt_consp-fluid_type.           " P1EK020371
*   Get consumtion:                                         " P1EK020371
    IF rifltcons-recdf <> gt_consp-recdf.                   " P1EK020371
      gt_consp-recdf = rifltcons-recdf.
    ENDIF.                                                  " P1EK020371
*   Get fluid type:                                         " P1EK020371
    IF rifltcons-fluid_type <> gt_consp-fluid_type.         " P1EK020371
      gt_consp-fluid_type = rifltcons-fluid_type.           " P1EK020371
    ENDIF.                                                  " P1EK020371
    l_index = sy-stepl + g_consp_top_line - 1.
    MODIFY gt_consp INDEX l_index.
    g_check_req = c_x.
  ENDIF.


ENDFORM.                               " CONSP_PAI

*&---------------------------------------------------------------------*
*&      Form  COUNT_PAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM count_pai.

  DATA: l_index LIKE sy-stepl.

  IF rifltcoun-recnt <> gt_count-recnt.
    gt_count-recnt = rifltcoun-recnt.
    l_index = sy-stepl + g_count_top_line - 1.
    MODIFY gt_count INDEX l_index.
    g_check_req = c_x.
  ENDIF.

ENDFORM.                               " COUNT_PAI

*&---------------------------------------------------------------------*
*&      Form  CALL_CAL1_POPUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_cal1_popup.

  CALL FUNCTION 'ITOB_CONSUMPTION_WINDOW'
    EXPORTING
      i_equnr = itob-equnr.

ENDFORM.                               " CALL_CAL1_POPUP

*---------------------------------------------------------------------*
*       FORM user_command_0200_exit                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM user_command_0200_exit.

  DATA: l_answer.

  IF NOT sy-datar IS INITIAL OR
     NOT g_dirty  IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        defaultoption = 'N'
        textline1     = TEXT-001
        textline2     = TEXT-002
        titel         = TEXT-000
      IMPORTING
        answer        = l_answer.
    CASE l_answer.
      WHEN 'J'.
*-> OK leave without SAVING
        CLEAR g_dirty.
        PERFORM user_command_0200.
      WHEN OTHERS.
*-> do nothing
        CLEAR g_ok_code.
    ENDCASE.
  ELSE.
    PERFORM user_command_0200.
  ENDIF.

ENDFORM.                    "user_command_0200_exit

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0200.

  DATA: l_answer.

  IF ( g_ok_code = 'CANC' OR
       g_ok_code = 'BACK' OR
       g_ok_code = 'LEAV' ) AND
   NOT g_dirty IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_DATA_LOSS'
      EXPORTING
        defaultoption = 'J'
        titel         = TEXT-000
      IMPORTING
        answer        = l_answer.
    CASE l_answer.
      WHEN 'N'.
*-> OK leave without SAVING
      WHEN 'J'.
        g_ok_code = 'SAVE'.
      WHEN OTHERS.
*-> do nothing
        CLEAR g_ok_code.
    ENDCASE.
  ENDIF.
*-> COMMAND
  CASE g_ok_code.
    WHEN 'CANC' OR
         'BACK'.
      PERFORM leave_to_start.
    WHEN 'LEAV'.
      PERFORM leave_to_caller.
    WHEN 'SAVE'.
      PERFORM update_counter.
    WHEN 'CAL1'.
      PERFORM call_cal1_popup.
    WHEN 'OIFO'.
      PERFORM view_fleet_object_info.
    WHEN 'IK22'.
      PERFORM more_meas_docs USING itob-equnr
                                   g_post_date
                                   g_post_time.
    WHEN 'IE03' OR
         'IW21'.
      SET PARAMETER ID 'EQN' FIELD itob-equnr.
      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'                 "v n2325737
        EXPORTING
          tcode  = g_ok_code
        EXCEPTIONS
          ok     = 1
          not_ok = 2
          OTHERS = 3.
      IF sy-subrc = 1.
        CALL TRANSACTION g_ok_code AND SKIP FIRST SCREEN.
      ELSE.
        MESSAGE ID '00' TYPE 'S' NUMBER '172' WITH g_ok_code.
      ENDIF.                                                "^ n2325737
    WHEN tzs1_fcode_button. " time zone button
      PERFORM time_zone_button_pressed.

  ENDCASE.

ENDFORM.                               " USER_COMMAND_0200

*&---------------------------------------------------------------------*
*&      Form  READ_T370FLD_STN_T
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_t370fld_stn_t.

*  IF t370fld_stn_t-mandt IS INITIAL.
    SELECT SINGLE * FROM t370fld_stn_t
                    WHERE station = t370fld_stn_t-station AND
                          lang_key = sy-langu.
    IF sy-subrc <> 0.
      t370fld_stn_t-mandt = sy-mandt.
*   Keine Buchung von Materialbelegen ohne gültige Tankstelle möglich
      MESSAGE s010.
    ELSE.
      g_station = c_x.
    ENDIF.
*  ENDIF.

ENDFORM.                               " READ_T370FLD_STN_T

*&---------------------------------------------------------------------*
*&      Form  READ_T370FLD_STN_T_STAT_T
*&---------------------------------------------------------------------*
*       Check if Char4 Gas station exists
*       Fills the char3 key field of gas station
*----------------------------------------------------------------------*
FORM read_t370fld_stn.

*  IF t370fld_stn_t-mandt IS INITIAL.
    IF t370fld_stn-station_t IS INITIAL.
*--- no gas station entered
      t370fld_stn_t-mandt = sy-mandt.
*--- Updating of material documents not possible without valid gas stations
      MESSAGE s010(itobfltcon).
    ELSE.
      SELECT * FROM t370fld_stn INTO t370fld_stn
               UP TO 1 ROWS
               WHERE station_t = t370fld_stn-station_t.
      ENDSELECT.
      IF sy-subrc <> 0.
        MESSAGE e012(itobfltcon) WITH t370fld_stn-station_t.
      ELSE.
*--- fill key of gas station
        t370fld_stn_t-mandt   = sy-mandt.
        t370fld_stn_t-station = t370fld_stn-station.
        g_station = c_x.
      ENDIF.
    ENDIF.
*  ENDIF.

ENDFORM.                               " READ_T370FLD_STN


*&---------------------------------------------------------------------*
*&      Form  READ_LAST_COUNTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_last_counter.

*-> fill rest of COUNTER
  LOOP AT gt_count.
*-> last value
    CALL FUNCTION 'ITOB_CONSUMPTION_COUNT_LCNTC'
      EXPORTING
        i_rifltcoun = gt_count
        i_post_date = g_post_date
        i_post_time = g_post_time
      IMPORTING
        e_rifltcoun = gt_count.
    MODIFY gt_count.
  ENDLOOP.

ENDFORM.                               " READ_LAST_COUNTER

*&---------------------------------------------------------------------*
*&      Form  VIEW_FLEET_OBJECT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_fleet_object_info.

  DATA: l_riwo1 LIKE riwo1.

  l_riwo1-equnr = itob-equnr.
  CALL FUNCTION 'PM_OBJECT_INFO'
    EXPORTING
*     ALWAYS    = 'X'
      info_wind = 'X'
      service   = 'X'
*     KDAUF     = ' '
*     KDPOS     = ' '
      i_riwo1   = l_riwo1.

ENDFORM.                               " VIEW_FLEET_OBJECT_INFO

*&---------------------------------------------------------------------*
*&      Form  more_meas_docs
*&---------------------------------------------------------------------*
*       additional measurement documents
*----------------------------------------------------------------------*
*      -->P_EQUNR Equipment
*      -->P_DATE  Date
*      -->P_TIME  Time
*----------------------------------------------------------------------*
FORM more_meas_docs  USING    p_equnr LIKE equi-equnr
                              p_date  LIKE sy-datum
                              p_time  LIKE sy-uzeit.

  DATA: lt_rimr0c LIKE TABLE OF rimr0c,
        ls_rimr0c LIKE rimr0c.

  CALL FUNCTION 'OBJECT_NUMBER_READ_IE'
    EXPORTING
      equnr            = p_equnr
    IMPORTING
      objnr            = ls_rimr0c-mpobj
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  APPEND ls_rimr0c TO lt_rimr0c.

  CALL FUNCTION 'MEASUREM_DOCUM_DIALOG_LIST'
    EXPORTING
      activity_type          = '1'
      f11_active             = ' '
      indicator_initialize   = ' '
      default_date           = p_date
      default_time           = p_time
*     DEFAULT_READER         = SY-UNAME
*     WORK_ORDER_OBJECTNR    = ' '
*     WORK_ORDER_KEY_EXTERN  = ' '
*     WORK_ORDER_OBJECTNR1   = ' '
*   IMPORTING
*     INDICATOR_UPDATE       =
*     LEFT_WITH_F15          =
    TABLES
      mp_objects             = lt_rimr0c
    EXCEPTIONS
      no_authority           = 1
      work_order_not_found   = 2
      order_has_no_documents = 3
      object_not_found       = 4
      object_has_no_points   = 5
      OTHERS                 = 6.

ENDFORM.                    " more_meas_docs

*&---------------------------------------------------------------------*
*&      Form  dynpro_init_0220
*&---------------------------------------------------------------------*
*       Initialisation of consuption sreen
*----------------------------------------------------------------------*
FORM dynpro_init_0220 .

  DATA:
    l_tabstrip_tab TYPE ito0t_tabstrip_tab,
    l_rec_equi     TYPE equi,
    l_rec_eqkt     TYPE eqkt,
    l_rec_equz     TYPE equz,
    l_rec_iloa     TYPE iloa.

  CLEAR g_ok_code.

  IF g_0220_init IS INITIAL.
    CALL FUNCTION 'ITOB_MOVE_DATA'
      EXPORTING
        i_move_to_itob  = itob_bool-false
        i_activity_type = itob_activity-display
      CHANGING
        c_itob          = itob
        c_equi          = l_rec_equi
        c_eqkt          = l_rec_eqkt
        c_equz          = l_rec_equz
        c_iloa          = l_rec_iloa.

    CALL FUNCTION 'ITOB_DATA_IMPORT'
      EXPORTING
        i_object_type    = itob_type-equi
        i_active_tabcode = space
        i_activity_type  = itob_activity-display
      TABLES
        t_tabstrip_tab   = l_tabstrip_tab
      CHANGING
        i_rec_equi       = l_rec_equi
        i_rec_eqkt       = l_rec_eqkt
        i_rec_equz       = l_rec_equz
        i_rec_iloa       = l_rec_iloa
        i_rec_fleet      = fleet.
    g_0220_init = itob_bool-true.
  ENDIF.

ENDFORM.                    " dynpro_init_0220

*&---------------------------------------------------------------------*
*&      Form  LOAD_STATION_TEXT
*&---------------------------------------------------------------------*
*       Form to load the text for Gas station
*----------------------------------------------------------------------*
FORM load_station_text .

  CONSTANTS: lc_lang_en   TYPE sylangu VALUE 'E'.

  DATA : lt_t370fld_stn_t TYPE TABLE OF t370fld_stn_t,
         ls_t370fld_stn_t LIKE LINE OF lt_t370fld_stn_t.

  IF t370fld_stn_t-station IS NOT INITIAL.
    g_station = c_x.
*   Get the Gas station text stored in all langauges
    SELECT * FROM t370fld_stn_t INTO TABLE lt_t370fld_stn_t
             WHERE station = t370fld_stn_t-station
             ORDER BY PRIMARY KEY.
    IF sy-subrc EQ 0.     " Entries are available in atleast one langauge
*    get the gas station text for default logon langauge
      READ TABLE lt_t370fld_stn_t INTO ls_t370fld_stn_t
                                  WITH KEY lang_key = sy-langu
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        station_text = ls_t370fld_stn_t-type_text.
      ELSE.
        CLEAR ls_t370fld_stn_t.
*      get the gas station text for english langauge if the text is not maintained
*      in logon langauge
        READ TABLE lt_t370fld_stn_t INTO ls_t370fld_stn_t
                                    WITH KEY lang_key = lc_lang_en
                                    BINARY SEARCH.
        IF sy-subrc EQ 0.
          station_text = ls_t370fld_stn_t-type_text.
        ELSE.
          CLEAR ls_t370fld_stn_t.
*      get the gas station text from any available langauge if the text is not maintained
*      in either logon langauge or english langauge.
          READ TABLE lt_t370fld_stn_t INTO ls_t370fld_stn_t INDEX 1.
          IF sy-subrc EQ 0.
            station_text = ls_t370fld_stn_t-type_text.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
*     if there is no text available in any langauge
      CLEAR station_text.
    ENDIF.
  ENDIF.

ENDFORM.                    " LOAD_STATION_TEXT
