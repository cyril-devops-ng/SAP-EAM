FUNCTION zitob_consumption_update.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_EQUNR) TYPE  ITOB-EQUNR
*"     VALUE(I_DATE) TYPE  SY-DATLO DEFAULT SY-DATLO
*"     VALUE(I_TIME) TYPE  SY-TIMLO DEFAULT SY-TIMLO
*"     VALUE(I_PRI_CONSP_VAL) TYPE  RIFLTCONS-RECDF OPTIONAL
*"     VALUE(I_PRI_COUNT_VAL) TYPE  RIFLTCOUN-RECNT OPTIONAL
*"     VALUE(I_STATION) TYPE  T370FLD_STN-STATION OPTIONAL
*"     VALUE(I_IGNORE_GMOVE_ERR) LIKE  IREF-IIND DEFAULT 'X'
*"     VALUE(I_NO_T370CON_TOL_CHECK) LIKE  IREF-IIND OPTIONAL
*"     VALUE(I_CHECK_ONLY) LIKE  IREF-IIND OPTIONAL
*"  EXPORTING
*"     VALUE(E_ERR_POINT) TYPE  IMRC_POINT
*"  TABLES
*"      T_CONSP STRUCTURE  RIFLTCONS OPTIONAL
*"      T_COUNT STRUCTURE  RIFLTCOUN OPTIONAL
*"  EXCEPTIONS
*"      FLEET_DATA_NOT_COMPLETE
*"      NO_STATION
*"      NO_T370FLD_GMOVE_ENTRY
*"      GMOVE_ERROR
*"      VALUES_MISSING
*"----------------------------------------------------------------------
  DATA l_pri_consp_pt         TYPE imrc_point.
  DATA l_pri_count_pt         TYPE imrc_point.
  DATA t_impt                 LIKE impt OCCURS 0 WITH HEADER LINE.
  DATA l_station              TYPE c LENGTH 1.
  DATA l_check_tol            TYPE c LENGTH 1.
  DATA t_t370fld_mat          LIKE t370fld_mat OCCURS 0 WITH HEADER LINE.
  DATA l_bapihead             LIKE bapi2017_gm_head_01.
  DATA l_bapicode             LIKE bapi2017_gm_code.
  DATA l_bapiheadret          LIKE bapi2017_gm_head_ret.
  DATA t_bapiitem             LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE.
  DATA t_bapiret              LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
  DATA l_prps                 LIKE prps.
  DATA l_value_num            LIKE rimr01-recdv.
  DATA l_value_num_2          LIKE rimr01-recdv.
  DATA l_value_min_char       LIKE rimr0-readc.
  DATA l_value_max_char       LIKE rimr0-readc.
  DATA l_imrg_pri_consp_cdiff TYPE imrc_cdiff.
  DATA l_imrg_pri_count_cdiff TYPE imrc_cdiff.
  DATA l_imrg_pri_count_readg TYPE imrc_readg.
  DATA l_rimr01_def           LIKE rimr01.
  DATA t_rimr01               LIKE rimr01 OCCURS 0 WITH HEADER LINE.
  DATA t_i_imrg               LIKE imrg OCCURS 0 WITH HEADER LINE.

  DATA ls_eam_ifcu            LIKE eam_ifcu.
  " TODO: variable is assigned but never used (ABAP cleaner)
  DATA ls_eam_ifcu_db         LIKE eam_ifcu.
  DATA lv_message             TYPE c LENGTH 1 ##NEEDED.

*break csayeh.
  " -> read object data
  CALL FUNCTION 'ITOB_CONSUMPTION_MPPOS_GET'
    EXPORTING  i_equnr            = i_equnr
    IMPORTING  e_itob             = itob
               e_fleet            = fleet
               e_pri_consp_pt     = l_pri_consp_pt
               e_pri_count_pt     = l_pri_count_pt
    TABLES     t_mpoints          = t_impt
    EXCEPTIONS not_found          = 1
               no_fleet_object    = 2
               no_measurem_points = 3
               OTHERS             = 4.
  IF sy-subrc <> 0.
    RAISE fleet_data_not_complete.
  ENDIF.

  " -> check type
  SELECT SINGLE * FROM t370con_tol
    WHERE tol_key = fleet-consump_tol.
  " -> rule found ?
  IF    l_pri_consp_pt IS INITIAL
     OR l_pri_count_pt IS INITIAL.
*    IF NOT t370con_tol-msgty IS INITIAL AND
*       i_no_t370con_tol_check IS INITIAL.
*      MESSAGE e??? RAISING fleet_data_not_complete.
*    ENDIF.
  ELSE.
    " -> is check necessary
    IF     t370con_tol-msgty      IS NOT INITIAL
       AND i_no_t370con_tol_check IS INITIAL.
      l_check_tol = c_x.
    ENDIF.
    " -> check whether direct values are given
    IF i_pri_consp_val IS NOT INITIAL.
      " -> add/mod consp. line
      READ TABLE t_consp WITH KEY point = l_pri_consp_pt.
      IF sy-subrc <> 0.
        t_consp-recdf = i_pri_consp_val.
        t_consp-point = l_pri_consp_pt.
        READ TABLE t_impt WITH KEY point = l_pri_consp_pt.
        IF sy-subrc = 0.
          t_consp-msehi = t_impt-msehi.
        ENDIF.
        APPEND t_consp.
      ELSE.
        t_consp-recdf = i_pri_consp_val.
        MODIFY t_consp INDEX sy-tabix.
      ENDIF.
    ENDIF.
    IF i_pri_count_val IS NOT INITIAL.
      " -> add/mod counter line
      READ TABLE t_count WITH KEY point = l_pri_count_pt.
      t_count-recnt = i_pri_count_val.
      IF sy-subrc <> 0.
        t_count-recnt = i_pri_count_val.
        t_count-point = l_pri_count_pt.
        READ TABLE t_impt WITH KEY point = l_pri_count_pt.
        IF sy-subrc = 0.
          t_count-msehi = t_impt-msehi.
        ENDIF.
        APPEND t_count.
      ELSE.
        t_count-recnt = i_pri_count_val.
        MODIFY t_count INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDIF.

  " -> read Customizing for Goods Movements --------------
  " -> Station
  IF t370fld_stn-mandt IS INITIAL.
    SELECT SINGLE * FROM t370fld_stn
      WHERE station = i_station.
    IF sy-subrc <> 0.
      IF i_ignore_gmove_err IS INITIAL.
        RAISE no_station.
      ENDIF.
    ELSE.
      l_station = c_x.
    ENDIF.
  ELSE.
    l_station = c_x.
  ENDIF.

  " -> indicated: create Goods Movements
  IF l_station IS NOT INITIAL.
    " -> read T370FLD_GMOVE
    SELECT SINGLE * FROM t370fld_gmove
      WHERE gmove_key = fleet-consump_move.
    IF sy-subrc <> 0.
      IF i_ignore_gmove_err IS INITIAL.
        MESSAGE e011 RAISING no_t370fld_gmove_entry.
      ENDIF.
      CLEAR l_station.
    ENDIF.
    IF l_station IS NOT INITIAL.
      " -> T370FLD_MAT in internal table
      SELECT * INTO TABLE t_t370fld_mat
        FROM t370fld_mat
        WHERE station = t370fld_stn-station.
      " -> prepare BAPI structure
      l_bapihead-doc_date   = i_date.
      l_bapihead-pstng_date = l_bapihead-doc_date.

      l_bapicode-gm_code = t370fld_gmove-gm_code.

      t_bapiitem-move_type = t370fld_gmove-bwartwa.
      t_bapiitem-plant     = t370fld_stn-plant.
      t_bapiitem-stge_loc  = t370fld_stn-storage.
      " -> account
      IF t370fld_gmove-ac_costc IS NOT INITIAL.
        t_bapiitem-costcenter = itob-kostl.
      ENDIF.
      IF t370fld_gmove-ac_wbs_e IS NOT INITIAL.
        CALL FUNCTION 'CJPN_GET_WBS_ELEMENT'
          EXPORTING  i_pspnr     = itob-proid
          IMPORTING  e_prps      = l_prps
          EXCEPTIONS input_error = 1
                     not_found   = 2
                     OTHERS      = 3.
        IF sy-subrc = 0.
          t_bapiitem-wbs_elem = l_prps-posid.
        ENDIF.
      ENDIF.
      IF t370fld_gmove-ac_asset IS NOT INITIAL.
        t_bapiitem-asset_no   = itob-anlnr.
        t_bapiitem-sub_number = itob-anlun.
      ENDIF.
      IF t370fld_gmove-ac_order IS NOT INITIAL.
        t_bapiitem-orderid = itob-aufnr.
      ELSEIF t370fld_gmove-ac_st_order IS NOT INITIAL.
        t_bapiitem-orderid = itob-daufn.
      ENDIF.
    ENDIF.
  ENDIF.

  " -> fill default values for measurement doc function module
  l_rimr01_def-idate = i_date.
  l_rimr01_def-itime = i_time.
  l_rimr01_def-readr = sy-uname.
  l_rimr01_def-gener = 'C'.            " Consumption
  " -> Check CONSUMPTION
  LOOP AT t_consp.
    IF t_consp-recdf IS INITIAL.
      CONTINUE.
    ENDIF.

    " -> refresh/clear parameter
    REFRESH t_rimr01.
    t_rimr01 = l_rimr01_def.
    " -> convert unit
    IF t_consp-msehi IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_LUNIT_INPUT'
        EXPORTING  input          = t_consp-mseh6
        IMPORTING  output         = t_rimr01-recdu
        EXCEPTIONS unit_not_found = 1.
      IF sy-subrc <> 0.
        e_err_point = t_consp-point.
        EXIT.
      ELSE.
        t_consp-msehi = t_rimr01-recdu.
      ENDIF.
    ELSE.
      t_rimr01-recdu = t_consp-msehi.
    ENDIF.
    " -> convert value to FLTP
    CALL FUNCTION 'CHAR_FLTP_CONVERSION'
      EXPORTING  string          = t_consp-recdf
      IMPORTING  flstr           = t_rimr01-recdv
                 ivalu           = t_rimr01-recdvi
      EXCEPTIONS string_not_fltp = 1.
    IF sy-subrc <> 0.
      e_err_point = t_consp-point.
      EXIT.
    ENDIF.
    " -> meas. point
    t_rimr01-point = t_consp-point.
    " -> consumption as diff
    t_rimr01-idiff = c_x.
    " -> create table entry
    APPEND t_rimr01.
    " -> don't call function in check mode
    IF i_check_only IS INITIAL.
      " -> create document
      CALL FUNCTION 'MEASUREM_DOCUM_DIALOG_LIST_1'
        EXPORTING  f11_active          = c_x
                   default_date        = i_date
                   default_time        = i_time
                   no_dialog           = c_x
        TABLES     documents_default   = t_rimr01
                   documents_insert    = t_i_imrg
        EXCEPTIONS no_authority        = 1
                   imptt_not_found     = 4
                   type_not_found      = 5
                   point_locked        = 6
                   point_inactive      = 7
                   timestamp_in_future = 8
                   timestamp_duprec    = 9
                   unit_unfit          = 10
                   value_overflow      = 11
                   value_unfit         = 12
                   value_missing       = 13
                   code_not_found      = 14.
      IF sy-subrc <> 0.
        e_err_point = t_consp-point.
        EXIT.
      ENDIF.
    ENDIF.
    IF l_station IS NOT INITIAL.
      " -> add entry for goods movement
      t_bapiitem-entry_qnt = t_rimr01-recdv.
      t_bapiitem-entry_uom = t_rimr01-recdu.
      IF t_consp-fluid_mat IS NOT INITIAL.
        " t_bapiitem-material = t_consp-fluid_mat. "MFLE
        t_bapiitem-material_long = t_consp-fluid_mat. " MFLE
      ELSE.
        PERFORM det_fluid_type USING    fleet
                               CHANGING t_consp.
        IF t_consp-fluid_type IS INITIAL.
          IF i_ignore_gmove_err IS INITIAL.
            e_err_point = t_consp-point.
            " Es konnte kein Betriebstoff ermittelt werden.
            MESSAGE s020.
            EXIT.
          ENDIF.
        ELSE.
          READ TABLE t_t370fld_mat
               WITH KEY fluid_type = t_consp-fluid_type
                        station    = t370fld_stn-station
                        def_mat    = c_x.
          IF sy-subrc <> 0.
            IF i_ignore_gmove_err IS INITIAL.
              e_err_point = t_consp-point.
              " Es konnte kein Material zum Betriebstoff ermittelt werden.
              MESSAGE s021.
              EXIT.
            ENDIF.
          ELSE.
            " t_bapiitem-material = t_t370fld_mat-matnr. "MFLE
            t_bapiitem-material_long = t_t370fld_mat-matnr. " MFLE
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND t_bapiitem.
    ENDIF.
    " -> save result for check against the counter
    IF t_consp-point = l_pri_consp_pt.
      IF i_check_only IS INITIAL.
        READ TABLE t_i_imrg WITH KEY point = t_consp-point.
        l_imrg_pri_consp_cdiff = t_i_imrg-cdiff.
      ELSE.
        CALL FUNCTION 'CHAR_FLTP_CONVERSION_TO_SI'
          EXPORTING  char_value      = t_consp-recdf
                     char_unit       = t_consp-msehi
          IMPORTING  fltp_value_si   = l_imrg_pri_consp_cdiff
          EXCEPTIONS no_unit_given   = 1
                     string_not_fltp = 2
                     OTHERS          = 3.
        IF sy-subrc <> 0.
          e_err_point = t_consp-point.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  " -> check for Error
  IF e_err_point IS NOT INITIAL.
    EXIT.
  ENDIF.

  " -> call BAPI for Goods Movements in Test Modus
  IF     l_station          IS NOT INITIAL
     AND i_ignore_gmove_err IS INITIAL
     AND t_bapiitem[]       IS NOT INITIAL.
    " -> Test mode
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING goodsmvt_header  = l_bapihead
                goodsmvt_code    = l_bapicode
                testrun          = c_x
      IMPORTING goodsmvt_headret = l_bapiheadret
      TABLES    goodsmvt_item    = t_bapiitem
                return           = t_bapiret.
    READ TABLE t_bapiret INDEX 1.
    IF sy-subrc = 0.
      PERFORM bapiret2_to_syst USING t_bapiret.
      READ TABLE t_consp INDEX t_bapiret-row.
      IF sy-msgty = 'A' OR sy-msgty = 'E' OR sy-msgty = 'X'. " v N2417661
        e_err_point = t_consp-point.
        EXIT.
      ELSEIF sy-msgty = 'I' OR sy-msgty = 'S' OR sy-msgty = 'W'.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.    "^ N2417661
      ENDIF.
    ENDIF.
  ENDIF.

  " -> Check COUNTER
  LOOP AT t_count.
    IF t_count-recnt IS INITIAL.
      CONTINUE.
    ENDIF.

    " -> refresh/clear parameter
    REFRESH t_rimr01.
    t_rimr01 = l_rimr01_def.
    " -> convert unit
    IF t_count-msehi IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_LUNIT_INPUT'
        EXPORTING  input          = t_count-mseh6
        IMPORTING  output         = t_rimr01-recdu
        EXCEPTIONS unit_not_found = 1.
      IF sy-subrc <> 0.
        e_err_point = t_count-point.
        EXIT.
      ELSE.
        t_count-msehi = t_rimr01-recdu.
      ENDIF.
    ELSE.
      t_rimr01-recdu = t_count-msehi.
    ENDIF.
    " -> convert value to FLTP
    CALL FUNCTION 'CHAR_FLTP_CONVERSION'
      EXPORTING  string          = t_count-recnt
      IMPORTING  flstr           = t_rimr01-recdv
                 ivalu           = t_rimr01-recdvi
      EXCEPTIONS string_not_fltp = 1.
    IF sy-subrc <> 0.
      e_err_point = t_count-point.
      EXIT.
    ENDIF.
    " -> meas. point
    t_rimr01-point = t_count-point.
    " -> create table entry
    APPEND t_rimr01.
    " -> don't call function in check mode
    IF i_check_only IS INITIAL.
      " -> create/update document
      CALL FUNCTION 'MEASUREM_DOCUM_DIALOG_LIST_1'
        EXPORTING  f11_active          = c_x
                   default_date        = i_date
                   default_time        = i_time
                   no_dialog           = c_x
        TABLES     documents_default   = t_rimr01
                   documents_insert    = t_i_imrg
        EXCEPTIONS no_authority        = 1
                   imptt_not_found     = 4
                   type_not_found      = 5
                   point_locked        = 6
                   point_inactive      = 7
                   timestamp_in_future = 8
                   timestamp_duprec    = 9
                   unit_unfit          = 10
                   value_overflow      = 11
                   value_unfit         = 12
                   value_missing       = 13
                   code_not_found      = 14.
      IF sy-subrc <> 0.
        e_err_point = t_count-point.
        EXIT.
      ENDIF.
    ENDIF.
    " -> save result and check against average
    IF i_check_only IS INITIAL.
      IF t_count-point = l_pri_count_pt.
        READ TABLE t_i_imrg WITH KEY point = t_count-point.
        l_imrg_pri_count_cdiff = t_i_imrg-cdiff.
        l_imrg_pri_count_readg = t_i_imrg-readg.
      ENDIF.
    ELSE.
      " -> determine SI value for entered measurement
      CALL FUNCTION 'CHAR_FLTP_CONVERSION_TO_SI'
        EXPORTING  char_value      = t_count-recnt
                   char_unit       = t_count-msehi
        IMPORTING  fltp_value_si   = l_value_num
        EXCEPTIONS no_unit_given   = 1
                   string_not_fltp = 2
                   OTHERS          = 3.
      IF sy-subrc <> 0.
        e_err_point = t_count-point.
        EXIT.
      ENDIF.
      " -> det. SI value for last measure
      CALL FUNCTION 'CHAR_FLTP_CONVERSION_TO_SI'
        EXPORTING  char_value      = t_count-lcntc
                   char_unit       = t_count-msehi
        IMPORTING  fltp_value_si   = l_value_num_2
        EXCEPTIONS no_unit_given   = 1
                   string_not_fltp = 2
                   OTHERS          = 3.
      IF sy-subrc <> 0.
        e_err_point = t_count-point.
        EXIT.
      ENDIF.

      " --- actual counter < last counter ?
      IF l_value_num < l_value_num_2.
        READ TABLE t_impt WITH KEY point = t_count-point.
        " --- if counter overflow -> add to actual counter
        IF t_impt-cjumpi IS NOT INITIAL.
          l_value_num += t_impt-cjump.
          MESSAGE w036(ir) WITH t_count-point.
        ENDIF.
      ENDIF.

      " -> calc. difference
      l_value_num_2 = l_value_num - l_value_num_2.
      " -> must be more than 0
      IF l_value_num_2 <= 0.
        MESSAGE s031.
        e_err_point = t_count-point.
        EXIT.
      ENDIF.
      " -> if primary counter, store values for T370CON_TOL check
      IF t_count-point = l_pri_count_pt.
        l_imrg_pri_count_readg = l_value_num.
        l_imrg_pri_count_cdiff = l_value_num_2.
      ENDIF.
    ENDIF.
    " -> perform check against T370CON_TOL
    IF     l_check_tol            IS NOT INITIAL
       AND l_imrg_pri_count_cdiff IS NOT INITIAL.
      " -> check readings
      IF l_imrg_pri_consp_cdiff IS INITIAL.
        MESSAGE s032.
        " -> find missing point for consumption
        READ TABLE t_consp WITH KEY point = l_pri_consp_pt.
        IF sy-subrc = 0.
          e_err_point = t_consp-point.
        ELSE.
          e_err_point = t_count-point.
        ENDIF.
        EXIT.
      ENDIF.
      " -> perform check
      PERFORM check_t370con_tol
        TABLES   t_impt
        USING    itob
                 fleet
                 l_imrg_pri_count_cdiff
                 l_pri_count_pt
                 l_imrg_pri_count_readg
                 t_count-decim
                 t_count-msehi
                 l_imrg_pri_consp_cdiff
                 l_pri_consp_pt
        CHANGING sy-subrc
                 l_value_min_char
                 l_value_max_char.
      IF sy-subrc <> 0.
        " -> display message
        CASE sy-subrc.
          WHEN 2.
            MESSAGE s040 WITH l_value_min_char
                              l_value_max_char
                              t_count-mseh6.
          WHEN 3.
            MESSAGE s041 WITH l_value_min_char
                              t_count-mseh6.
          WHEN 4.
            MESSAGE s042 WITH l_value_max_char
                              t_count-mseh6.
        ENDCASE.
        " -> no Error if not enough meaurments
        IF sy-subrc <> 5.
          " -> in check mode we raise always
          IF    i_check_only      IS NOT INITIAL
*-> is it not a message which type is in T370CON_TOL?
             OR sy-subrc           = 1
             OR t370con_tol-msgty  = 'E'.
            e_err_point = t_count-point.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
      " -> check was performed
      CLEAR l_check_tol.
    ENDIF.
  ENDLOOP.

  " -> check active and not all values given?
  IF     l_check_tol IS NOT INITIAL
     AND e_err_point IS INITIAL
     AND (    t370con_tol-msgty  = 'E'
           OR i_check_only      IS NOT INITIAL ).
    IF l_imrg_pri_consp_cdiff IS INITIAL.
      " -> find missing point for consumption
      READ TABLE t_consp WITH KEY point = l_pri_consp_pt.
      IF sy-subrc = 0.
        e_err_point = t_consp-point.
        MESSAGE s032.
        EXIT.
      ENDIF.
    ELSEIF l_imrg_pri_count_cdiff IS INITIAL.
      " -> find missing point for prim. counter
      READ TABLE t_count WITH KEY point = l_pri_count_pt.
      IF sy-subrc = 0.
        e_err_point = t_count-point.
        MESSAGE s044.
        EXIT.
      ENDIF.
    ELSE.
      " -> this should never happen!
      MESSAGE i043 RAISING values_missing.
    ENDIF.
  ENDIF.

  " -> call BAPI for Goods Movements in DB Modus
  IF     l_station    IS NOT INITIAL
     AND e_err_point  IS INITIAL
     AND i_check_only IS INITIAL
     AND t_bapiitem[] IS NOT INITIAL.
    CLEAR t_bapiret.
    REFRESH t_bapiret.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING goodsmvt_header  = l_bapihead
                goodsmvt_code    = l_bapicode
                testrun          = space
      IMPORTING goodsmvt_headret = l_bapiheadret
      TABLES    goodsmvt_item    = t_bapiitem
                return           = t_bapiret.
    " -> check error
    IF     l_bapiheadret      IS INITIAL
       AND i_ignore_gmove_err IS INITIAL.
      READ TABLE t_bapiret INDEX 1.
      PERFORM bapiret2_to_syst USING t_bapiret.
      READ TABLE t_consp INDEX t_bapiret-row.
      e_err_point = t_consp-point.
      " -> unexpected error -> test mode was successful!
      RAISE gmove_error.
    ENDIF.

    break csayeh.
    " IFCU Reverse Consumption functionality
    IF cl_ops_switch_check=>eam_sfws_ui_fleet( ) = abap_true.
      " add reference of goods movement to technical object
      ls_eam_ifcu-objnr        = itob-objnr.
      ls_eam_ifcu-posting_date = i_date.
      ls_eam_ifcu-posting_time = i_time.
      ls_eam_ifcu-mblnr        = l_bapiheadret-mat_doc.
      ls_eam_ifcu-mjahr        = l_bapiheadret-doc_year.
      ls_eam_ifcu-doc_type     = 'G'.

      " check if there exist already an entry.
      SELECT SINGLE * FROM eam_ifcu
        INTO ls_eam_ifcu_db
        WHERE objnr        = ls_eam_ifcu-objnr
          AND posting_date = ls_eam_ifcu-posting_date
          AND posting_time = ls_eam_ifcu-posting_time.
      " if there exist an entry raise error, because there can only be one measurement document for
      " a measurement point at a time/date combination. This means that also only one goods movement can
      " exist to a time/date combination.
      IF sy-subrc IS INITIAL.
        MESSAGE e050(itobfltcon) RAISING gmove_error.
        " if there exist no entry to the time/date combination, insert it in database
      ELSE.
        CALL FUNCTION 'EAM_IFCU_UPDATE' IN UPDATE TASK
          EXPORTING is_eam_ifcu = ls_eam_ifcu.
        WAIT UP TO 2 SECONDS.
        IF xsdbool( ls_eam_ifcu-mblnr IS NOT INITIAL
        AND ls_eam_ifcu-mjahr IS NOT INITIAL )
           = abap_true.
          DATA lt_equi_no TYPE TABLE OF zmm_equip_no.
          clear lt_equi_no.
          loop at t_bapiitem ASSIGNING FIELD-SYMBOL(<fs_bapi_item>).
            append initial line to lt_equi_no ASSIGNING FIELD-SYMBOL(<fs_equi_no>).
            <fs_equi_no> = value #( mblnr = ls_eam_ifcu-mblnr
                                    mjahr = ls_eam_ifcu-mjahr
                                    zeile = sy-tabix
                                    zzequnr = I_EQUNR
                                    ).
          endloop.

          IF lines( lt_equi_no ) > 0.
            MODIFY zmm_equip_no FROM TABLE lt_equi_no.
          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
