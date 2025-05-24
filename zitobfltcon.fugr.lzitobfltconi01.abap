" -----------------------------------------------------------------------
" INCLUDE LITOBFLTCONI01 .
" -----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE g_ok_code.
    WHEN 'LEAV' OR
         'BACK'.
      PERFORM leave_to_caller.
    WHEN 'CANC'.
      PERFORM leave_to_start.
    WHEN space OR
         'STRT'.
      CLEAR lt_msg.
      CLEAR lt_text.

      go_t_editor->get_text_as_r3table( IMPORTING table = lt_text ).
      CLEAR gf_remarks.
      LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
        gf_remarks = |{ gf_remarks } { <fs_text> }|.
      ENDLOOP.
      " -------------------------------------------------------------
      " Get Staff ID for user
      " ------------------------------------------------------------
      DATA(lf_user) = cl_abap_context_info=>get_user_technical_name( ).
      DATA(lf_current_date) = cl_abap_context_info=>get_system_date( ).
      SELECT SINGLE FROM pa0105
        FIELDS pernr
        WHERE usrid
              = @lf_user
          AND endda >=
              @lf_current_date
        INTO @gf_attendant_id.

      IF gf_attendant_id IS INITIAL.
        TRY.
            RAISE EXCEPTION NEW zcx_eam_exception( textid     = zcx_eam_exception=>eam_user_staff_not_assigned
                                                   eam_userid = CONV #( lf_user ) ).
          CATCH zcx_eam_exception INTO DATA(lo_x).
            lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_x ).
            zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
            RETURN.
        ENDTRY.
      ENDIF.
      if gf_staff_id is not initial.
        select single from
        pa0001
        fields
        @abap_true
        where pernr
        = @gf_staff_id
        into @data(lf_staff_exists).
        if lf_staff_exists eq abap_false.
          TRY.
            RAISE EXCEPTION NEW zcx_eam_exception( textid     = zcx_eam_exception=>eam_staffdoesnotexist
                                                   eam_staff = conv #(  gf_staff_id ) ).
          CATCH zcx_eam_exception INTO lo_x.
            lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_x ).
            zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
            RETURN.
        ENDTRY.
        endif.
      endif.
      TRY.
          IF gf_file IS INITIAL.
            DATA(lo_eam_ifcu) = zcl_eam_ifcu=>create_ifcu_consumption(
                                    equnr        = itob-equnr
                                    station      = t370fld_stn_t-station
                                    posting_date = cl_abap_context_info=>get_system_date( )
                                    posting_time = cl_abap_context_info=>get_system_time( )
                                    staff_id     = gf_attendant_id
                                    short_text   = gf_remarks
*                                    attachment   =
*                                    attachment_ttl =
                                    ifcu_process = zcl_eam_ifcu=>gc_new_fuel_consumption ).
          ELSE.
            lo_eam_ifcu = zcl_eam_ifcu=>create_ifcu_consumption(
                              equnr          = itob-equnr
                              station        = t370fld_stn_t-station
                              posting_date   = cl_abap_context_info=>get_system_date( )
                              posting_time   = cl_abap_context_info=>get_system_time( )
                              staff_id       = gf_attendant_id
                              short_text     = gf_remarks
                              attachment     = gf_file
                              attachment_ttl = CONV #( gf_doc_title )
                              ifcu_process   = zcl_eam_ifcu=>gc_new_fuel_consumption ).
          ENDIF.
          PERFORM start_main_screen.
        CATCH zcx_eam_exception INTO DATA(lo_eam_cx).
          lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_eam_cx ).
          zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
      ENDTRY.
    WHEN 'ODCR'. " show documents to technical object
      lcl_selection_screen=>call_selection_screen( ).
      CLEAR g_ok_code.
    WHEN 'NONSTF'.
      IF gf_non_staff = abap_true.
        SELECT SINGLE
          FROM zifcu_c_non_staf AS non_staff
                 INNER JOIN
                   pa0002 ON  non_staff~pernr
                              = pa0002~pernr
                          AND endda >= @sy-datum
          FIELDS non_staff~pernr,
                 concat_with_space( pa0002~vorna, pa0002~nachn , 1 ) AS cname
          WHERE non_staff~status
                = @abap_true
          INTO ( @gf_staff_id,@gf_staff_name ).

        IF gf_staff_id IS INITIAL.
          TRY.
              RAISE EXCEPTION NEW zcx_eam_exception( textid    = zcx_eam_exception=>eam_gen_error
                                                     eam_error = 'No non-sfaff collector defined.' ).
            CATCH zcx_eam_exception INTO DATA(lo_non_stf_ex).
              CLEAR lt_msg.
              lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_non_stf_ex ).
              zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
              gf_non_staff = space.

          ENDTRY.
        ENDIF.
      ELSE.
        CLEAR: gf_staff_id,
               gf_staff_name.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  PERFORM user_command_0200.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  INIT_EQUI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_equi INPUT.

  PERFORM init_equi.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_COUNTER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_counter INPUT.

  PERFORM check_counter.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CONSP_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE consp_pai INPUT.

  PERFORM consp_pai.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  COUNT_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE count_pai INPUT.

  PERFORM count_pai.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ITOB_FCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE itob_fcode INPUT.

  CALL FUNCTION 'ITOB_FCODE_IMPORT'
    EXPORTING i_active_fcode = g_ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200_exit INPUT.

  PERFORM user_command_0200_exit.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  READ_STATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_station INPUT.

  PERFORM read_t370fld_stn_t.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  READ_STATION_T  INPUT
*&---------------------------------------------------------------------*
*   form to read the gas station
*----------------------------------------------------------------------*
MODULE read_station_t INPUT.

  PERFORM read_t370fld_stn.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0220 INPUT.

  CASE g_ok_code.

    WHEN 'RW'.
      CLEAR g_0220_init.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  NEW_DATE_TIME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE new_date_time INPUT.

  PERFORM read_last_counter.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_STAFF_NAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

*&---------------------------------------------------------------------*
*&      Module  IMPORT_FILE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE import_file.
  DATA gt_filetable TYPE filetable.
  DATA lf_rc        TYPE i.
  CONSTANTS gc_max_file_name_len TYPE i VALUE 128.
  CLEAR lt_msg.
  cl_gui_frontend_services=>file_open_dialog( EXPORTING  window_title            = |Supporting file selection|
                                                         initial_directory       = `/`
                                              CHANGING   file_table              = gt_filetable
                                                         rc                      = lf_rc
                                              EXCEPTIONS file_open_dialog_failed = 1
                                                         cntl_error              = 2
                                                         error_no_gui            = 3
                                                         not_supported_by_gui    = 4
                                                         OTHERS                  = 5 ).
  IF line_exists( gt_filetable[ 1 ] ).
    gf_file = gt_filetable[ 1 ].
    IF xsdbool( strlen( gt_filetable[ 1 ] ) > gc_max_file_name_len ) = abap_true.
      TRY.
          RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_filepathtoolong ).
        CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
          CLEAR gf_file.
          lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
          zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
      ENDTRY.
    ENDIF.
  ENDIF.
  " ---------------------------------------------------------------------
  " 60: Check: file type
  " ---------------------------------------------------------------------
  IF xsdbool(  contains( val  = to_lower( gf_file )
                         pcre = `.pdf|.jpg|.png` ) ) = abap_false.
    TRY.
        RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_unsupportedfileformat ).
      CATCH zcx_eam_exception INTO lo_cx_eam.
        CLEAR gf_file.
        lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_cx_eam ).
        zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
    ENDTRY.
  ENDIF.
ENDMODULE.                             " IMPORT_FILE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_STATION_LIST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_station_list INPUT.
  " ---------------------------------------------------------------------
  " 10: Data Declaration
  " ---------------------------------------------------------------------
  TYPES: BEGIN OF s_station_sel,
           station_t    TYPE station_t,
           station_name TYPE stn_type_text,
         END OF s_station_sel.
  TYPES tr_station_t TYPE RANGE OF station_t.
  DATA lt_station_sel TYPE TABLE OF s_station_sel.
  DATA lt_return_tab  TYPE hrreturn_tab.
  DATA go_eam_ifcu_s  TYPE REF TO zcl_eam_ifcu.
  DATA lf_equnr       TYPE equi-equnr.
  DATA lt_dyn_fld     TYPE TABLE OF dynpread.

  " ---------------------------------------------------------------------
  " 10.1 Get cursor field equipment
  " ---------------------------------------------------------------------
  GET CURSOR FIELD itob-equnr.
  CLEAR lf_equnr.
  INSERT VALUE #( fieldname = 'ITOB-EQUNR' ) INTO TABLE lt_dyn_fld.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING dyname     = sy-cprog
              dynumb     = sy-dynnr
    TABLES    dynpfields = lt_dyn_fld.

  IF xsdbool( sy-subrc = 0 AND line_exists( lt_dyn_fld[ 1 ] ) ) = abap_true.
    lf_equnr = lt_dyn_fld[ 1 ]-fieldvalue.
  ENDIF.
  " ---------------------------------------------------------------------
  " 10.2: Raise exception if equipment field is empty
  " ---------------------------------------------------------------------
  IF lf_equnr IS INITIAL.
    TRY.
        RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_equipment_is_mandatory ).
      CATCH zcx_eam_exception INTO DATA(lo_equnr_ex).
        DATA(lt_e_msg) = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_equnr_ex ).
        zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_e_msg ).
    ENDTRY.
    CLEAR lt_e_msg.
  ENDIF.
  CHECK lf_equnr IS NOT INITIAL.
  go_eam_ifcu_s = NEW #( ).
  " -------------------------------------------------------------
  " 20: Get Staff ID for user
  " ------------------------------------------------------------
  lf_user = cl_abap_context_info=>get_user_technical_name( ).
  lf_current_date = cl_abap_context_info=>get_system_date( ).
  SELECT SINGLE FROM pa0105
    FIELDS pernr
    WHERE usrid
          = @lf_user
      AND endda >=
          @lf_current_date
    INTO @gf_attendant_id.

  IF gf_attendant_id IS INITIAL.
    TRY.
        RAISE EXCEPTION NEW zcx_eam_exception( textid     = zcx_eam_exception=>eam_user_staff_not_assigned
                                               eam_userid = CONV #( lf_user ) ).
      CATCH zcx_eam_exception INTO lo_x.
        lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_x ).
        zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
        RETURN.
    ENDTRY.
  ENDIF.

  CHECK gf_attendant_id IS NOT INITIAL.
  " ---------------------------------------------------------------------
  " 30: Get staff access
  " ---------------------------------------------------------------------
  DATA(lt_staff_access) =
  go_eam_ifcu_s->get_staff_access(
      ir_staff_id            = VALUE #( ( low = gf_attendant_id sign = 'I' option = 'EQ' ) )
      if_include_temp_access = abap_true ).
  " ---------------------------------------------------------------------
  " 40: Assign station access to search help structure
  " ---------------------------------------------------------------------
  lt_station_sel = CORRESPONDING #( lt_staff_access ).
  " ---------------------------------------------------------------------
  " 50: Get fluid type for equipment
  " ---------------------------------------------------------------------
  SELECT SINGLE
    FROM equi
           INNER JOIN
             fleet ON equi~objnr
                      = fleet~objnr
    FIELDS fleet~fuel_pri AS fluid_type
    WHERE equi~equnr
          = @lf_equnr
    INTO @DATA(lf_fluid_type).
  " ---------------------------------------------------------------------
  " 60: Get stations with this fluid types
  " ---------------------------------------------------------------------
  SELECT
    FROM T370FLD_mat
           INNER JOIN
             T370FLD_stn ON T370FLD_mat~station
                            = T370FLD_stn~station
    FIELDS T370FLD_mat~fluid_type,
           T370FLD_stn~station_t
    WHERE T370FLD_mat~fluid_type
          = @lf_fluid_type
    INTO TABLE @DATA(lt_fluid_stations).
  " ---------------------------------------------------------------------
  " 70: Delete stations without the fluid types
  " ---------------------------------------------------------------------
  DELETE lt_station_sel
         WHERE station_t NOT IN VALUE tr_station_t( FOR <fs> IN lt_fluid_stations
                                                    ( low = <fs>-station_t sign = 'I' option = 'EQ' ) ).
  " ---------------------------------------------------------------------
  " 80:Display the search help
  " ---------------------------------------------------------------------
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING  retfield        = 'STATION_T'
               value_org       = 'S'
               window_title    = `Station selection`
    TABLES     value_tab       = lt_station_sel
               return_tab      = lt_return_tab
    EXCEPTIONS parameter_error = 1
               no_values_found = 2
               OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID     sy-msgid
            TYPE   sy-msgty
            NUMBER sy-msgno
            WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  " ---------------------------------------------------------------------
  " 90: Assign the search help value
  " ---------------------------------------------------------------------
  CHECK line_exists( lt_return_tab[ 1 ] ).
  t370fld_stn-station_t = lt_return_tab[ 1 ]-fieldval.
ENDMODULE.
