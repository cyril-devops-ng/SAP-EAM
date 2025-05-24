CLASS zcl_eam_ifcu_gen DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_new_sacc,
             staff_id TYPE persno,
             station  TYPE station,
           END OF t_new_sacc.
    TYPES: BEGIN OF t_new_stac,
             staff_id      TYPE persno,
             station       TYPE station,
             valid_from    TYPE begda,
             valid_to      TYPE endda,
             access_reason TYPE text255,
           END OF t_new_stac.
    TYPES: BEGIN OF t_new_ifcu,
             equnr        TYPE equnr,
             staff_id     TYPE persno,
             station      TYPE station,
             posting_date TYPE imrc_idate,
             posting_time TYPE imrc_itime,
             short_text   TYPE text255,
           END OF t_new_ifcu.
    TYPES: BEGIN OF t_draw_doc,
             dokar TYPE dokar,
             doknr TYPE doknr,
             dokvr TYPE dokvr,
             doktl TYPE doktl_d,
           END OF t_draw_doc.

    CLASS-METHODS display_bapi_log_gui
      IMPORTING it_bapi_ret TYPE bapiret2_t.

    CLASS-METHODS display_station_selection
      IMPORTING it_assigned_stations TYPE zcl_eam_ifcu=>tt_stations_list
      RETURNING VALUE(rt_stations)   TYPE zcl_eam_ifcu=>tt_stations_list.

    CLASS-METHODS display_location_selection
      IMPORTING it_assigned_locations TYPE zcl_eam_ifcu=>tt_locations_list
      RETURNING VALUE(rt_locations)   TYPE zcl_eam_ifcu=>tt_locations_list.

    CLASS-METHODS create_stations_from_locations
      IMPORTING it_locations       TYPE zcl_eam_ifcu=>tt_locations_list
      RETURNING VALUE(rt_stations) TYPE zcl_eam_ifcu=>tt_stations_list.

    CLASS-METHODS create_bapi_ret_from_exception
      IMPORTING io_exception  TYPE REF TO cx_root
      RETURNING VALUE(rt_msg) TYPE bapiret2_t.

    CLASS-METHODS process_field_validation
      IMPORTING io_eam_ifcu TYPE REF TO zcl_eam_ifcu
      RAISING   zcx_eam_exception.

    CLASS-METHODS process_sacc_f_validation
      IMPORTING io_eam_ifcu TYPE REF TO zcl_eam_ifcu
      RAISING   zcx_eam_exception.

    CLASS-METHODS process_stac_f_validation
      IMPORTING io_eam_ifcu TYPE REF TO zcl_eam_ifcu
      RAISING   zcx_eam_exception.

    CLASS-METHODS process_ifcu_f_validation
      IMPORTING io_eam_ifcu TYPE REF TO zcl_eam_ifcu
      RAISING   zcx_eam_exception.

    CLASS-METHODS retrieve__gas_station
      IMPORTING if_service_station    TYPE station
      RETURNING VALUE(rf_gas_station) TYPE station_t.

    CLASS-METHODS retrieve__service_station
      IMPORTING if_gas_station            TYPE station_t
      RETURNING VALUE(rf_service_station) TYPE station.

    CLASS-METHODS retrieve_fleet
      IMPORTING if_equnr        TYPE equnr
      RETURNING VALUE(rs_fleet) TYPE fleet.

    CLASS-METHODS validate_fuel_type
      IMPORTING if_equnr       TYPE equnr
                if_fuel_type   TYPE fluid_type
      RETURNING VALUE(rf_flag) TYPE flag
      RAISING   zcx_eam_exception.

    CLASS-METHODS update_etag
      IMPORTING io_eam_ifcu    TYPE REF TO zcl_eam_ifcu
      RETURNING VALUE(rs_etag) TYPE zficu_etag.

    CLASS-METHODS eam_file_upload
      IMPORTING io_eam_ifcu        TYPE REF TO zcl_eam_ifcu
      RETURNING VALUE(rs_draw_doc) TYPE t_draw_doc
      RAISING   zcx_eam_exception.

ENDCLASS.


CLASS zcl_eam_ifcu_gen IMPLEMENTATION.
  METHOD create_bapi_ret_from_exception.
    " ---------------------------------------------------------------------
    " 10: Create BAPI return from Exception
    " ---------------------------------------------------------------------
    CALL FUNCTION 'RS_EXCEPTION_TO_BAPIRET2'
      EXPORTING i_r_exception = io_exception
      CHANGING  c_t_bapiret2  = rt_msg.
  ENDMETHOD.

  METHOD display_bapi_log_gui.
    " ---------------------------------------------------------------------
    " 10: Display Bapi return
    " ---------------------------------------------------------------------
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING it_message = it_bapi_ret.
  ENDMETHOD.

  METHOD display_station_selection.
    " ---------------------------------------------------------------------
    " 10: Type pools
    " ---------------------------------------------------------------------
    TYPE-POOLS slis.
    TYPES tr_stations TYPE RANGE OF station.
    " ---------------------------------------------------------------------
    " 20: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_fieldcatalog TYPE slis_t_fieldcat_alv.
    DATA lf_check_exit   TYPE flag.
    " ---------------------------------------------------------------------
    " 30: Get stations
    " ---------------------------------------------------------------------
    SELECT *
      FROM t370fld_stn AS stn
             INNER JOIN
               t370fld_stn_t AS stn_t ON stn~station =
                                         stn_t~station
      WHERE lang_key
            = 'E'
      INTO CORRESPONDING FIELDS OF TABLE
      @rt_stations.
    " ---------------------------------------------------------------------
    " 40: Only show stations not yet assigned
    " ---------------------------------------------------------------------
    IF it_assigned_stations IS NOT INITIAL.
      DELETE rt_stations
             WHERE station IN
                   VALUE tr_stations( FOR <fs> IN it_assigned_stations
                                      ( low    = <fs>-station
                                        sign   = 'I'
                                        option = 'EQ' ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 40: Create field catalog
    " ---------------------------------------------------------------------
    INSERT LINES OF VALUE slis_t_fieldcat_alv( tabname = 'RT_STATIONS'
                                               ( fieldname = 'FLAG'
                                                 col_pos   = 1
                                                 seltext_l = `Select`
                                                 outputlen = 3
                                                 input     = abap_true
                                                 edit      = abap_true )
                                               ( fieldname = 'STATION'
                                                 col_pos   = 2
                                                 seltext_l = `Service Station`
                                                 outputlen = 7 )
                                               ( fieldname = 'STATION_T'
                                                 col_pos   = 3
                                                 seltext_l = `Gas Station`
                                                 outputlen = 7 )
                                               ( fieldname = 'TYPE_TEXT'
                                                 col_pos   = 4
                                                 seltext_l = `Station Name`
                                                 outputlen = 40 ) )
           INTO TABLE lt_fieldcatalog.
    " ---------------------------------------------------------------------
    " 50: Display Pop-Up
    " ---------------------------------------------------------------------
    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING  i_title                 = `Service Station Selection`
                 i_selection             = abap_true
                 i_zebra                 = 'X'
                 i_screen_start_column   = 5
                 i_screen_start_line     = 5
                 i_screen_end_column     = 50
                 i_screen_end_line       = 40
                 i_tabname               = 'RT_STATIONS'
                 i_checkbox_fieldname    = 'FLAG'
                 i_scroll_to_sel_line    = 'X'
                 it_fieldcat             = lt_fieldcatalog
                 i_callback_program      = sy-repid
                 i_callback_user_command = 'USER_COMMAND'
      IMPORTING  e_exit                  = lf_check_exit
      TABLES     t_outtab                = rt_stations
      EXCEPTIONS program_error           = 1.
    IF lf_check_exit = abap_true.
      CLEAR rt_stations.
    ENDIF.
  ENDMETHOD.

  METHOD process_field_validation.
    " ---------------------------------------------------------------------
    " 10: Process validations
    " ---------------------------------------------------------------------
    CASE io_eam_ifcu->gs_ifcu_auth-ifcu_proc.
      " ---------------------------------------------------------------------
      " 20: IFCU validations
      " ---------------------------------------------------------------------
      WHEN zcl_eam_ifcu=>gc_proc_ifcu.
        process_ifcu_f_validation( io_eam_ifcu  ).
        " ---------------------------------------------------------------------
        " 30: Staff Temporary Assignment validations
        " ---------------------------------------------------------------------
      WHEN zcl_eam_ifcu=>gc_proc_sacc.
        process_sacc_f_validation( io_eam_ifcu ).
        " ---------------------------------------------------------------------
        " 40: Staff Assignment validations
        " ---------------------------------------------------------------------
      WHEN zcl_eam_ifcu=>gc_proc_stac.
        process_stac_f_validation( io_eam_ifcu ).
    ENDCASE.
  ENDMETHOD.

  METHOD process_ifcu_f_validation.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lo_struc TYPE REF TO cl_abap_structdescr.

    " ---------------------------------------------------------------------
    " 20: Describe components
    " ---------------------------------------------------------------------
    lo_struc ?= cl_abap_structdescr=>describe_by_data( VALUE t_new_ifcu( ) ).
    " ---------------------------------------------------------------------
    " 30: Check components
    " ---------------------------------------------------------------------
    DO lines( lo_struc->components ) TIMES.
      ASSIGN lo_struc->components[ sy-index ] TO FIELD-SYMBOL(<fs_component>).
      CASE <fs_component>-name.
        " ---------------------------------------------------------------------
        " 40: Equipment No. check
        " ---------------------------------------------------------------------
        WHEN `EQUNNR`.
          IF xsdbool( io_eam_ifcu->gf_equnr IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_equipment_is_mandatory ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 50: Station check
          " ---------------------------------------------------------------------
        WHEN 'STATION'.
          IF xsdbool( io_eam_ifcu->gf_station IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_station_is_mandatory ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 60: Staff ID check
          " ---------------------------------------------------------------------
        WHEN 'STAFF_ID'.
          IF xsdbool( io_eam_ifcu->gf_staff_id IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_staffid_is_mandatory ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 70: Short text check
          " ---------------------------------------------------------------------
        WHEN 'SHORT_TEXT'.
          IF xsdbool( ( io_eam_ifcu->gf_short_text IS INITIAL
              OR io_eam_ifcu->gf_short_text = space )
              AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
              = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_shorttext_is_mandatory ).
          ENDIF.
      ENDCASE.
    ENDDO.
    " ---------------------------------------------------------------------
    " 80: Check for station exists
    " ---------------------------------------------------------------------
    SELECT SINGLE FROM t370fld_stn
      FIELDS @abap_true
      WHERE station =
            @io_eam_ifcu->gf_station
      INTO @DATA(lf_station_exists).
    " ---------------------------------------------------------------------
    " 90: Raise exception
    " ---------------------------------------------------------------------
    IF lf_station_exists = abap_false.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid      = zcx_eam_exception=>eam_station_doesnotexist
                           eam_station = retrieve__gas_station( io_eam_ifcu->gf_station ) ).
    ENDIF.

    " ---------------------------------------------------------------------
    " 100: Check if equipment exists
    " ---------------------------------------------------------------------
    SELECT SINGLE FROM equi
      FIELDS @abap_true
      WHERE equnr =
            @io_eam_ifcu->gf_equnr
      INTO @DATA(lf_equipment_exists).
    " ---------------------------------------------------------------------
    " 110: Raise exception
    " ---------------------------------------------------------------------
    IF lf_equipment_exists = abap_false.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid    = zcx_eam_exception=>eam_equipment_doesnotexist
                           eam_fleet = CONV #( io_eam_ifcu->gf_equnr ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 120: Check employee exists (Personal Details) #EC_NEEDED
    " ---------------------------------------------------------------------
    SELECT SINGLE FROM pa0002
      FIELDS @abap_true
      WHERE pernr =
            @io_eam_ifcu->gf_staff_id
      INTO @DATA(lf_staff_exists).
    " ---------------------------------------------------------------------
    " 130: Raise exception
    " ---------------------------------------------------------------------
    IF lf_staff_exists = abap_false.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid    = zcx_eam_exception=>eam_staffdoesnotexist
                           eam_staff = CONV #( io_eam_ifcu->gf_staff_id ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 140: Check if staff is assigned to station
    " ---------------------------------------------------------------------
    SELECT FROM zifcu_pa_station
      FIELDS pernr   AS Staff_id,
             station AS Service_Station
      WHERE pernr   = @io_eam_ifcu->gf_staff_id
        AND station = @io_eam_ifcu->gf_station
    UNION ALL
    SELECT FROM zifcu_pa_sta_tac
      FIELDS pernr   AS Staff_id,
             station AS Service_Station
      WHERE pernr     = @io_eam_ifcu->gf_staff_id
        AND station   = @io_eam_ifcu->gf_station
        AND valid_to >= @sy-datum
        AND status    = @abap_true
    INTO TABLE @DATA(lt_staff_access).
    " ---------------------------------------------------------------------
    " 150: Raise exception
    " ---------------------------------------------------------------------
    IF xsdbool( line_exists( lt_staff_access[ 1 ] ) )
       = abap_false.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid      = zcx_eam_exception=>eam_staffnotassignedtostation
                           eam_staff   = CONV #( io_eam_ifcu->gf_staff_id )
                           eam_station = retrieve__gas_station( io_eam_ifcu->gf_station ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 160: Check if fuel type for fleet is available at station
    " ---------------------------------------------------------------------
    DATA(ls_fleet) = retrieve_fleet( io_eam_ifcu->gf_equnr ).
    IF xsdbool( ls_fleet IS INITIAL )
       = abap_false.
      SELECT SINGLE FROM t370fld_mat
        FIELDS @abap_true
        WHERE station =
              @io_eam_ifcu->gf_station
          AND (
        fluid_type =
        @ls_fleet-fuel_pri
*        OR fluid_type =
*           @ls_fleet-fuel_sec
        )
        INTO @DATA(lf_fuel_type_exists).
      " ---------------------------------------------------------------------
      " 170: Raise exception
      " ---------------------------------------------------------------------
      IF lf_fuel_type_exists = abap_false.
        RAISE EXCEPTION NEW
          zcx_eam_exception( textid        = zcx_eam_exception=>eam_fueltypemismatchtank_fleet
                             eam_fuel_type = COND #( WHEN ls_fleet-fuel_sec
                                                          IS INITIAL
                                                     THEN |{ ls_fleet-fuel_pri }|
                                                     ELSE |{ ls_fleet-fuel_pri } or { ls_fleet-fuel_sec }| )
                             eam_station   = retrieve__gas_station( io_eam_ifcu->gf_station )
                             eam_fleet     = CONV #( io_eam_ifcu->gf_equnr ) ).
      ENDIF.
    ENDIF.
    " ---------------------------------------------------------------------
    " 180: Check for document title if file is provided
    " ---------------------------------------------------------------------
    IF xsdbool( io_eam_ifcu->gf_attachment IS NOT INITIAL
           AND (   io_eam_ifcu->gf_attachment_tl IS INITIAL
             OR io_eam_ifcu->gf_attachment_tl = space ) )
       = abap_true.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid = zcx_eam_exception=>eam_nodoc_title ).
    ENDIF.
  ENDMETHOD.

  METHOD process_sacc_f_validation.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lo_struc TYPE REF TO cl_abap_structdescr.

    " ---------------------------------------------------------------------
    " 20: Describe components
    " ---------------------------------------------------------------------
    lo_struc ?= cl_abap_structdescr=>describe_by_data( VALUE t_new_sacc( ) ).
    " ---------------------------------------------------------------------
    " 30: Check components
    " ---------------------------------------------------------------------
    DO lines( lo_struc->components ) TIMES.
      ASSIGN lo_struc->components[ sy-index ] TO FIELD-SYMBOL(<fs_component>).
      CASE <fs_component>-name.
        " ---------------------------------------------------------------------
        " 40: Station check
        " ---------------------------------------------------------------------
        WHEN 'STATION'.
          IF xsdbool( io_eam_ifcu->gf_station IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_station_is_mandatory ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 50: Staff ID check
          " ---------------------------------------------------------------------
        WHEN 'STAFF_ID'.
          IF xsdbool( io_eam_ifcu->gf_staff_id IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_staffid_is_mandatory ).
          ENDIF.
      ENDCASE.
    ENDDO.
    " ---------------------------------------------------------------------
    " 60.0 Additional checks for create
    " ---------------------------------------------------------------------
    IF io_eam_ifcu->gs_ifcu_auth-ifcu_actv <> zcl_eam_ifcu=>gc_create.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 60: Check employee exists (Personal Details) #EC_NEEDED
    " ---------------------------------------------------------------------
    SELECT SINGLE FROM pa0002
      FIELDS @abap_true
      WHERE pernr =
            @io_eam_ifcu->gf_staff_id
      INTO @DATA(lf_staff_exists).
    " ---------------------------------------------------------------------
    " 70: Raise exception
    " ---------------------------------------------------------------------
    IF lf_staff_exists = abap_false.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid    = zcx_eam_exception=>eam_staffdoesnotexist
                           eam_staff = CONV #( io_eam_ifcu->gf_staff_id ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 80: Check for station exists
    " ---------------------------------------------------------------------
    SELECT SINGLE FROM t370fld_stn
      FIELDS @abap_true
      WHERE station =
            @io_eam_ifcu->gf_station
      INTO @DATA(lf_station_exists).
    " ---------------------------------------------------------------------
    " 90: Raise exception
    " ---------------------------------------------------------------------
    IF lf_station_exists = abap_false.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid      = zcx_eam_exception=>eam_station_doesnotexist
                           eam_station = retrieve__gas_station( io_eam_ifcu->gf_station ) ).
    ENDIF.

    " Check: 100 REMOVED to allow Staff assignment to multiple locations

*    " ---------------------------------------------------------------------
*    " 100: Check if staff is already assigned to station
*    " ---------------------------------------------------------------------
*    SELECT SINGLE FROM zifcu_pa_station
*      FIELDS *
*      WHERE pernr    = @io_eam_ifcu->gf_staff_id
*        AND station <> @space
*      INTO @DATA(ls_staff_assignment).
*    " ---------------------------------------------------------------------
*    " 110: Raise exception
*    " ---------------------------------------------------------------------
*    IF ls_staff_assignment IS NOT INITIAL.
*      RAISE EXCEPTION NEW
*        zcx_eam_exception( textid      = zcx_eam_exception=>eam_staff_alreadyassigned
*                           eam_staff   = CONV #( io_eam_ifcu->gf_staff_id )
*                           eam_station = retrieve__gas_station( ls_staff_assignment-station ) ).
*    ENDIF.
  ENDMETHOD.

  METHOD process_stac_f_validation.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lo_struc TYPE REF TO cl_abap_structdescr.

    " ---------------------------------------------------------------------
    " 20: Describe components
    " ---------------------------------------------------------------------
    lo_struc ?= cl_abap_structdescr=>describe_by_data( VALUE t_new_stac( ) ).
    " ---------------------------------------------------------------------
    " 30: Check components
    " ---------------------------------------------------------------------
    DO lines( lo_struc->components ) TIMES.
      ASSIGN lo_struc->components[ sy-index ] TO FIELD-SYMBOL(<fs_component>).
      CASE <fs_component>-name.
        " ---------------------------------------------------------------------
        " 40: Station check
        " ---------------------------------------------------------------------
        WHEN 'STATION'.
          IF xsdbool( io_eam_ifcu->gf_station IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_station_is_mandatory ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 50: Staff ID check
          " ---------------------------------------------------------------------
        WHEN 'STAFF_ID'.
          IF xsdbool( io_eam_ifcu->gf_staff_id IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_staffid_is_mandatory ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 60: Temporary start date check
          " ---------------------------------------------------------------------
        WHEN 'VALID_FROM'.
          IF xsdbool( io_eam_ifcu->gf_valid_from IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_startdate_is_mandatory ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 70: Temporary end date check
          " ---------------------------------------------------------------------
        WHEN 'VALID_TO'.
          IF xsdbool( io_eam_ifcu->gf_valid_to IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_enddate_is_mandatory ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 80: Access Reason check
          " ---------------------------------------------------------------------
        WHEN 'ACCESS_REASON'.
          IF xsdbool( io_eam_ifcu->gf_access_reason IS INITIAL
               AND io_eam_ifcu->gs_ifcu_auth-ifcu_actv
               = zcl_eam_ifcu=>gc_create ) = abap_true.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_accessreason_is_mandatory ).
          ENDIF.

      ENDCASE.
    ENDDO.
    " ---------------------------------------------------------------------
    " 90: Check employee exists (Personal Details) #EC_NEEDED
    " ---------------------------------------------------------------------
    SELECT SINGLE FROM pa0002
      FIELDS @abap_true
      WHERE pernr =
            @io_eam_ifcu->gf_staff_id
      INTO @DATA(lf_staff_exists).
    " ---------------------------------------------------------------------
    " 100: Raise exception
    " ---------------------------------------------------------------------
    IF lf_staff_exists = abap_false.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid    = zcx_eam_exception=>eam_staffdoesnotexist
                           eam_staff = CONV #( io_eam_ifcu->gf_staff_id ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 110: Check for station exists
    " ---------------------------------------------------------------------
    SELECT SINGLE FROM t370fld_stn
      FIELDS @abap_true
      WHERE station =
            @io_eam_ifcu->gf_station
      INTO @DATA(lf_station_exists).
    " ---------------------------------------------------------------------
    " 120: Raise exception
    " ---------------------------------------------------------------------
    IF lf_station_exists = abap_false.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid      = zcx_eam_exception=>eam_station_doesnotexist
                           eam_station = retrieve__gas_station( io_eam_ifcu->gf_station ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 130: Check staff is already assigned to this station
    " or has an active temporary access to this station
    " ---------------------------------------------------------------------
    SELECT FROM zifcu_pa_station
      FIELDS pernr   AS Staff_Id,
             station AS Station
      WHERE pernr   = @io_eam_ifcu->gf_staff_id
        AND station = @io_eam_ifcu->gf_station

    UNION ALL

    SELECT FROM zifcu_pa_sta_tac
      FIELDS pernr   AS Staff_Id,
             station AS Station
      WHERE status    = @abap_true
        AND valid_to >= @sy-datum
        AND pernr     = @io_eam_ifcu->gf_staff_id
        AND station   = @io_eam_ifcu->gf_station
    INTO TABLE @DATA(lt_staff_assignments).

    IF lt_staff_assignments IS NOT INITIAL.
      RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_staff_alreadyassigned ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 140: Check if start date is earlier than current date
    " ---------------------------------------------------------------------
    IF io_eam_ifcu->gf_valid_from < cl_abap_context_info=>get_system_date( ).
      RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_startdate_earlier ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 150: Check if start date is later than end date
    " ---------------------------------------------------------------------
    IF io_eam_ifcu->gf_valid_from > io_eam_ifcu->gf_valid_to.
      RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_startdate_later ).
    ENDIF.
  ENDMETHOD.

  METHOD retrieve__service_station.
    " ---------------------------------------------------------------------
    " 10: Retrieve service station
    " ---------------------------------------------------------------------
    SELECT SINGLE FROM t370fld_stn
      FIELDS station
      WHERE station_t
            = @if_gas_station
      INTO @rf_service_station.
  ENDMETHOD.

  METHOD retrieve__gas_station.
    " ---------------------------------------------------------------------
    " 10: Retrieve gas station
    " ---------------------------------------------------------------------
    SELECT SINGLE FROM t370fld_stn
      FIELDS station_t
      WHERE station
            = @if_service_station
      INTO @rf_gas_station.
  ENDMETHOD.

  METHOD retrieve_fleet.
    " ---------------------------------------------------------------------
    " 10: Retrieve fleet info
    " ---------------------------------------------------------------------
    SELECT SINGLE
      FROM equi
             INNER JOIN
               fleet ON equi~objnr
                        = fleet~objnr
      FIELDS fleet~*
      WHERE equi~equnr
            = @if_equnr
      INTO CORRESPONDING FIELDS OF
      @rs_fleet.
  ENDMETHOD.

  METHOD validate_fuel_type.
    " ---------------------------------------------------------------------
    " 10: Retrieve fleet info
    " ---------------------------------------------------------------------
    DATA(ls_fleet)
        = retrieve_fleet( if_equnr ).
    " ---------------------------------------------------------------------
    " 20: Check entered fuel type is a primary/secondary fuel type for fleet
    " ---------------------------------------------------------------------
    IF xsdbool( ls_fleet-fuel_pri <> if_fuel_type
            AND ls_fleet-fuel_sec <> if_fuel_type )
       = abap_true.
      " ---------------------------------------------------------------------
      " 30: Raise exception
      " ---------------------------------------------------------------------
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid        = zcx_eam_exception=>eam_fueltypenotallowedforfleet
                           eam_fleet     = CONV #( if_equnr )
                           eam_fuel_type = CONV #( if_fuel_type ) ).
    ENDIF.
  ENDMETHOD.

  METHOD update_etag.
    " ---------------------------------------------------------------------
    " 10: Create ETAG obj
    " ---------------------------------------------------------------------
    DATA(gt_etag) = NEW zcl_eam_ifcu=>t_etag( ).
    " ---------------------------------------------------------------------
    " 20: Init. Etag
    " ---------------------------------------------------------------------
    INSERT INITIAL LINE INTO TABLE gt_etag->* ASSIGNING FIELD-SYMBOL(<fs_etag>).
    " ---------------------------------------------------------------------
    " 30: Check activity
    " ---------------------------------------------------------------------
    IF xsdbool( io_eam_ifcu->gs_ifcu_auth-ifcu_actv
       = zcl_eam_ifcu=>gc_create )
       = abap_false.
      RETURN.
    ENDIF.

    " ---------------------------------------------------------------------
    " 40: Get time stamp
    " ---------------------------------------------------------------------
    GET TIME STAMP FIELD DATA(lf_timestamp).
    " ---------------------------------------------------------------------
    " 50: Try.
    " ---------------------------------------------------------------------
    TRY.
        " ---------------------------------------------------------------------
        " 60: Get user's time zone
        " ---------------------------------------------------------------------
        DATA(lf_usr_tz) = cl_abap_context_info=>get_user_time_zone( ).
        " ---------------------------------------------------------------------
        " 70: Convert time zone into (date,time)
        " ---------------------------------------------------------------------
        CONVERT TIME STAMP lf_timestamp
                TIME ZONE  lf_usr_tz
                INTO DATE  DATA(lf_current_date)
                     " TODO: variable is assigned but never used (ABAP cleaner)
                TIME DATA(lf_current_time).
        " ---------------------------------------------------------------------
        " 80: Create etag
        " ---------------------------------------------------------------------
        <fs_etag> = VALUE #( created_by      = cl_abap_context_info=>get_user_technical_name( )
                             created_on      = lf_current_date
                             last_changed_by = cl_abap_context_info=>get_user_technical_name( )
                             last_changed_on = lf_current_date
                             last_changed_at = lf_timestamp ).
      CATCH cx_abap_context_info_error.
        " ---------------------------------------------------------------------
        " 90: Catch exception and create etag.
        " ---------------------------------------------------------------------
        <fs_etag> = VALUE #( created_by      = cl_abap_context_info=>get_user_technical_name( )
                             created_on      = cl_abap_context_info=>get_system_date( )
                             last_changed_by = cl_abap_context_info=>get_user_technical_name( )
                             last_changed_on = cl_abap_context_info=>get_system_date( )
                             last_changed_at = lf_timestamp ).
    ENDTRY.
    " ---------------------------------------------------------------------
    " 100: Update etag
    " ---------------------------------------------------------------------
    IF line_exists( gt_etag->*[ 1 ] ).
      ASSIGN gt_etag->*[ 1 ] TO <fs_etag>. " else unassign.
      IF xsdbool( sy-subrc = 0 AND  <fs_etag> IS ASSIGNED )
         = abap_false.
        RETURN.
      ENDIF.
      rs_etag = CORRESPONDING #( <fs_etag> ).
    ENDIF.
  ENDMETHOD.

  METHOD eam_file_upload.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_files    TYPE TABLE OF bapi_doc_files2.
    DATA ls_doc_data TYPE bapi_doc_draw2.
    DATA ls_msg      TYPE bapiret2.

    " ---------------------------------------------------------------------
    " 20: Define document data
    " ---------------------------------------------------------------------
    ls_doc_data = VALUE #( DocumentType  = 'ZPM'
                           Description   = io_eam_ifcu->gf_attachment_tl
                           UserName      = cl_abap_context_info=>get_user_technical_name( )
                           ValidFromDate = cl_abap_context_info=>get_system_date( ) ).
    " ---------------------------------------------------------------------
    " 30: Define file data
    " ---------------------------------------------------------------------
    DATA(file) = to_lower( io_eam_ifcu->gf_attachment ).
    INSERT VALUE bapi_doc_files2( originaltype    = '1'
                                  storagecategory = 'DMS_C1_ST'
                                  docfile         = io_eam_ifcu->gf_attachment
                                  wsapplication   = COND #(
                                        WHEN xsdbool( contains( val  = to_lower( file )
                                                                pcre = `.pdf` ) ) = abap_true THEN 'PDF'
                                        WHEN xsdbool( contains( val  = to_lower( file )
                                                                pcre = `.png` ) ) = abap_true THEN 'PNG'
                                        WHEN xsdbool( contains( val  = to_lower( file )
                                                                pcre = `.jpg` ) ) = abap_true THEN 'JPG' )
                                  description     = io_eam_ifcu->gf_attachment_tl
                                  checkedin       = space )
           INTO TABLE lt_files.
    " ---------------------------------------------------------------------
    " 40: Create document
    " ---------------------------------------------------------------------
    CALL FUNCTION 'BAPI_DOCUMENT_CREATE2'
      EXPORTING documentdata    = ls_doc_data
                pf_ftp_dest     = 'SAPFTPA'
                pf_http_dest    = 'SAPHTTPA'
      IMPORTING documenttype    = rs_draw_doc-dokar
                documentnumber  = rs_draw_doc-doknr
                documentpart    = rs_draw_doc-doktl
                documentversion = rs_draw_doc-dokvr
                return          = ls_msg.
    " ---------------------------------------------------------------------
    " 50: Check if error exists
    " ---------------------------------------------------------------------
    IF ls_msg-type CA `EAX`.
      CLEAR rs_draw_doc.
      " ---------------------------------------------------------------------
      " 60: Raise exception
      " ---------------------------------------------------------------------
      RAISE EXCEPTION NEW zcx_eam_exception( textid    = zcx_eam_exception=>eam_gen_error
                                             eam_error = CONV #( ls_msg-message ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 70: Commit document create
    " ---------------------------------------------------------------------
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = abap_true.
    " ---------------------------------------------------------------------
    " 80: Check-In Document
    " ---------------------------------------------------------------------
    CALL FUNCTION 'BAPI_DOCUMENT_CHECKIN2'
      EXPORTING documenttype    = rs_draw_doc-dokar
                documentnumber  = rs_draw_doc-doknr
                documentpart    = rs_draw_doc-doktl
                documentversion = rs_draw_doc-dokvr
      IMPORTING return          = ls_msg
      TABLES    documentfiles   = lt_files.
    " ---------------------------------------------------------------------
    " 90: Check if error exists
    " ---------------------------------------------------------------------
    IF ls_msg-type CA `EAX`.
      CLEAR rs_draw_doc.
      " ---------------------------------------------------------------------
      " 100: Raise exception
      " ---------------------------------------------------------------------
      RAISE EXCEPTION NEW zcx_eam_exception( textid    = zcx_eam_exception=>eam_gen_error
                                             eam_error = CONV #( ls_msg-message ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 110: Commit Check-in
    " ---------------------------------------------------------------------
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = abap_true.
  ENDMETHOD.

  METHOD display_location_selection.
    " ---------------------------------------------------------------------
    " 10: Type pools
    " ---------------------------------------------------------------------
    TYPE-POOLS slis.
    TYPES tr_location TYPE RANGE OF zeam_loc_code.
    " ---------------------------------------------------------------------
    " 20: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_fieldcatalog TYPE slis_t_fieldcat_alv.
    DATA lf_check_exit   TYPE flag.
    " ---------------------------------------------------------------------
    " 30: Get Locations
    " ---------------------------------------------------------------------
    SELECT FROM zifcu_locations AS loc
      FIELDS *
      INTO CORRESPONDING FIELDS OF TABLE
      @rt_locations.
    " ---------------------------------------------------------------------
    " 40: Only show locations not yet assigned
    " ---------------------------------------------------------------------
    IF it_assigned_locations IS NOT INITIAL.
      DELETE rt_locations
             WHERE loc_code IN
                   VALUE tr_location( FOR <fs> IN it_assigned_locations
                                      ( low    = <fs>-loc_code
                                        sign   = 'I'
                                        option = 'EQ' ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 40: Create field catalog
    " ---------------------------------------------------------------------
    INSERT LINES OF VALUE slis_t_fieldcat_alv( tabname = 'RT_LOCATIONS'
                                               ( fieldname = 'FLAG'
                                                 col_pos   = 1
                                                 seltext_l = `Select`
                                                 outputlen = 3
                                                 input     = abap_true
                                                 edit      = abap_true )
                                               ( fieldname = 'LOC_CODE'
                                                 col_pos   = 2
                                                 seltext_l = `Location Code`
                                                 outputlen = 7 )
                                               ( fieldname = 'LOC_NAME'
                                                 col_pos   = 4
                                                 seltext_l = `Location Name`
                                                 outputlen = 40 ) )
           INTO TABLE lt_fieldcatalog.
    " ---------------------------------------------------------------------
    " 50: Display Pop-Up
    " ---------------------------------------------------------------------
    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING  i_title                 = `IFCU Location Selection`
                 i_selection             = abap_true
                 i_zebra                 = 'X'
                 i_screen_start_column   = 5
                 i_screen_start_line     = 5
                 i_screen_end_column     = 50
                 i_screen_end_line       = 20
                 i_tabname               = 'RT_LOCATIONS'
                 i_checkbox_fieldname    = 'FLAG'
                 i_scroll_to_sel_line    = 'X'
                 it_fieldcat             = lt_fieldcatalog
                 i_callback_program      = sy-repid
                 i_callback_user_command = 'USER_COMMAND'
      IMPORTING  e_exit                  = lf_check_exit
      TABLES     t_outtab                = rt_locations
      EXCEPTIONS program_error           = 1.
    IF lf_check_exit = abap_true.
      CLEAR rt_locations.
    ENDIF.
  ENDMETHOD.

  METHOD create_stations_from_locations.
    TYPES tr_loc_code TYPE RANGE OF zeam_loc_code.

    DATA(lr_loc_code) = VALUE tr_loc_code( FOR <fs> IN it_locations
                                           WHERE ( flag = abap_true )
                                           ( sign   = 'I'
                                             option = 'EQ'
                                             low    = <fs>-loc_code ) ).
    SELECT
      FROM zifcu_statn_loc AS statn_loc
             INNER JOIN
               zifcu_locations AS locations ON statn_loc~loc_code
                                               = locations~loc_code
                 INNER JOIN
                   t370fld_stn AS station ON station~station =
                                             statn_loc~station
                     INNER JOIN
                       t370fld_stn_t AS
                       station_text ON station~station
                                       = station_text~station

      FIELDS @abap_true            AS flag,
             statn_loc~station      AS station,
             station~station_t      AS station_t,
             station_text~type_text AS type_text

      WHERE statn_loc~loc_code
            IN @lr_loc_code
      INTO CORRESPONDING FIELDS OF TABLE @rt_stations.
  ENDMETHOD.
ENDCLASS.
