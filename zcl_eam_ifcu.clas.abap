CLASS zcl_eam_ifcu DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_station_access,
             staff_id         TYPE persno,
             first_name       TYPE vorna,
             last_name        TYPE nachn,
             usnam            TYPE usnam,
             station          TYPE station,
             station_t        TYPE station_t,
             station_name     TYPE string,
             loc_name         TYPE zeam_loc_name,
             temporary_access TYPE flag,
             valid_from       TYPE begda,
             valid_to         TYPE endda,
           END OF t_station_access.
    TYPES: BEGIN OF t_stations_list,
             flag      TYPE char1,
             station   TYPE station,
             station_t TYPE station_t,
             type_text TYPE char40,
           END OF t_stations_list.
    TYPES: BEGIN OF t_locations_list,
             flag     TYPE char1,
             loc_code TYPE zeam_loc_code,
             loc_name TYPE zeam_loc_name,
           END OF t_locations_list.
    TYPES activity     TYPE char2.
    TYPES ifcu_process TYPE string.
    TYPES: BEGIN OF t_ifcu_auth,
             ifcu_proc TYPE zeam_ifcu_process,
             ifcu_actv TYPE activity,
           END OF t_ifcu_auth.
    TYPES tr_staff_id       TYPE RANGE OF persno.
    TYPES tr_first_name     TYPE RANGE OF vorna.
    TYPES tr_last_name      TYPE RANGE OF nachn.
    TYPES tr_station        TYPE RANGE OF station.
    TYPES tr_station_t      TYPE RANGE OF station_t.
    TYPES tr_station_name   TYPE RANGE OF stn_type_text.
    TYPES tr_valid_from     TYPE RANGE OF datum.
    TYPES tr_valid_to       TYPE RANGE OF datum.
    TYPES t_ifcu_pa_station TYPE STANDARD TABLE OF zifcu_pa_station WITH DEFAULT KEY.
    TYPES t_ifcu_pa_sta_tac TYPE STANDARD TABLE OF zifcu_pa_sta_tac WITH DEFAULT KEY.
    TYPES t_ifcu_tech_obj   TYPE STANDARD TABLE OF zifcu_tech_obj WITH DEFAULT KEY.
    TYPES t_eam_ifcu        TYPE STANDARD TABLE OF eam_ifcu WITH DEFAULT KEY.
    TYPES t_fleet           TYPE STANDARD TABLE OF fleet WITH DEFAULT KEY.
    TYPES t_stations        TYPE STANDARD TABLE OF t370fld_stn_t WITH DEFAULT KEY.
    TYPES tt_station_access TYPE STANDARD TABLE OF t_station_access WITH DEFAULT KEY.
    TYPES tt_stations_list  TYPE STANDARD TABLE OF t_stations_list WITH DEFAULT KEY.
    TYPES tt_locations_list TYPE STANDARD TABLE OF t_locations_list WITH DEFAULT KEY.
    TYPES t_etag            TYPE STANDARD TABLE OF zficu_etag WITH DEFAULT KEY.

    CLASS-METHODS create_temp_staff_assignment
      IMPORTING station         TYPE station
                staff_id        TYPE persno
                valid_from      TYPE begda
                valid_to        TYPE endda
                ifcu_process    TYPE zcl_eam_ifcu=>ifcu_process
                access_reason   TYPE zifcu_acc_reason
                if_temp_access  TYPE flag OPTIONAL
      RETURNING VALUE(r_result) TYPE REF TO zcl_eam_ifcu
      RAISING   zcx_eam_exception.

    CLASS-METHODS create_ifcu_consumption
      IMPORTING equnr           TYPE equnr
                station         TYPE station
                posting_date    TYPE imrc_idate
                posting_time    TYPE imrc_itime
                staff_id        TYPE persno
                short_text      TYPE text255
                attachment      TYPE filep OPTIONAL
                attachment_ttl  TYPE string OPTIONAL
                ifcu_process    TYPE zcl_eam_ifcu=>ifcu_process
      RETURNING VALUE(r_result) TYPE REF TO zcl_eam_ifcu
      RAISING   zcx_eam_exception.

    CLASS-METHODS create_staff_assignment
      IMPORTING station         TYPE station
                staff_id        TYPE persno
                if_temp_access  TYPE flag OPTIONAL
                ifcu_process    TYPE zcl_eam_ifcu=>ifcu_process
      RETURNING VALUE(r_result) TYPE REF TO zcl_eam_ifcu
      RAISING   zcx_eam_exception.

    DATA gs_ifcu_auth     TYPE t_ifcu_auth      READ-ONLY.
    DATA gf_equnr         TYPE equnr            READ-ONLY.
    DATA gf_station       TYPE station          READ-ONLY.
    DATA gf_posting_date  TYPE imrc_idate       READ-ONLY.
    DATA gf_posting_time  TYPE imrc_itime       READ-ONLY.
    DATA gf_staff_id      TYPE persno           READ-ONLY.
    DATA gf_short_text    TYPE text255          READ-ONLY.
    DATA gf_valid_from    TYPE begda            READ-ONLY.
    DATA gf_valid_to      TYPE endda            READ-ONLY.
    DATA gf_attachment    TYPE filep            READ-ONLY.
    DATA gf_access_reason TYPE zifcu_acc_reason READ-ONLY.
    DATA gf_attachment_tl TYPE string           READ-ONLY.
    DATA gs_etag          TYPE zficu_etag.

    METHODS constructor
      IMPORTING equnr         TYPE equnr                      OPTIONAL
                station       TYPE station                    OPTIONAL
                posting_date  TYPE imrc_idate                 OPTIONAL
                posting_time  TYPE imrc_itime                 OPTIONAL
                staff_id      TYPE persno                     OPTIONAL
                short_text    TYPE text255                    OPTIONAL
                valid_from    TYPE begda                      OPTIONAL
                valid_to      TYPE endda                      OPTIONAL
                ifcu_process  TYPE zcl_eam_ifcu=>ifcu_process OPTIONAL
                access_reason TYPE zifcu_acc_reason           OPTIONAL
                attachment    TYPE filep                      OPTIONAL
      RAISING   zcx_eam_exception.

    METHODS get_staff_access
      IMPORTING ir_staff_id            TYPE tr_staff_id     OPTIONAL
                ir_staff_first_name    TYPE tr_first_name   OPTIONAL
                ir_staff_last_name     TYPE tr_last_name    OPTIONAL
                ir_station             TYPE tr_station      OPTIONAL
                ir_station_t           TYPE tr_station_t    OPTIONAL
                ir_station_name        TYPE tr_station_name OPTIONAL
                ir_valid_from          TYPE tr_valid_from   OPTIONAL
                ir_valid_to            TYPE tr_valid_to     OPTIONAL
                if_include_temp_access TYPE flag

      RETURNING VALUE(rt_staff_access) TYPE tt_station_access
      RAISING   zcx_eam_exception.

    METHODS create_sacc
      RAISING zcx_eam_exception.

    METHODS revoke_sacc
      RAISING zcx_eam_exception.

    METHODS create_stac
      RAISING zcx_eam_exception.

    METHODS create_ifcu
      IMPORTING if_staff_id TYPE persno
      RAISING   zcx_eam_exception.

    CONSTANTS gc_new_staff_station_access    TYPE ifcu_process      VALUE 'NEW_STAFF_STATION'.
    CONSTANTS gc_revoke_staff_station_access TYPE ifcu_process      VALUE 'REVOKE_STAFF_STATION'.
    CONSTANTS gc_new_temp_staff_station_acc  TYPE ifcu_process      VALUE 'NEW_TEMP_STAFF_STATION'.
    CONSTANTS gc_revoke_temp_staff_stat_acc  TYPE ifcu_process      VALUE 'REVOKE_TEMP_STAFF_STATION'.
    CONSTANTS gc_get_staff_access            TYPE ifcu_process      VALUE 'RETRIEVE_STAFF_ACCESS'.
    CONSTANTS gc_get_temp_staff_access       TYPE ifcu_process      VALUE 'RETRIEVE_TEMP_STAFF_ACCESS'.
    CONSTANTS gc_new_fuel_consumption        TYPE ifcu_process      VALUE 'NEW_FUEL_CONSUMPTION'.
    CONSTANTS gc_get_fuel_consumption        TYPE ifcu_process      VALUE 'GET_FUEL_CONSUMPTION'.
    CONSTANTS gc_auth_error_stac_01          TYPE string            VALUE `Temporarily assign staff to service station`.
    CONSTANTS gc_auth_error_sacc_01          TYPE string            VALUE `Assign staff to service station`.
    CONSTANTS gc_auth_error_sacc_06          TYPE string            VALUE `Revoke staff assignment to service station`.
    CONSTANTS gc_auth_error_stac_06          TYPE string            VALUE `Revoke temporary staff to service station`.
    CONSTANTS gc_auth_error_sacc_03          TYPE string            VALUE `Display staff access`.
    CONSTANTS gc_auth_error_stac_03          TYPE string            VALUE `Display temporary staff access`.
    CONSTANTS gc_auth_error_ifcu_01          TYPE string            VALUE `Post fuel consumption`.
    CONSTANTS gc_auth_error_ifcu_03          TYPE string            VALUE `Display fuel consumption`.
    CONSTANTS gc_create                      TYPE activity          VALUE '01'.
    CONSTANTS gc_change                      TYPE activity          VALUE '02'.
    CONSTANTS gc_display                     TYPE activity          VALUE '03'.
    CONSTANTS gc_revoke                      TYPE activity          VALUE '06'.
    CONSTANTS gc_proc_sacc                   TYPE zeam_ifcu_process VALUE `SACC`.
    CONSTANTS gc_proc_stac                   TYPE zeam_ifcu_process VALUE `STAC`.
    CONSTANTS gc_proc_ifcu                   TYPE zeam_ifcu_process VALUE `IFCU`.

  PRIVATE SECTION.
    METHODS map_ifcu_process
      IMPORTING if_ifcu_process     TYPE ifcu_process
      RETURNING VALUE(rs_ifcu_auth) TYPE t_ifcu_auth
      RAISING   zcx_eam_exception.

    METHODS authorization_check
      IMPORTING is_ifcu_auth      TYPE t_ifcu_auth
      RETURNING VALUE(rf_granted) TYPE flag
      RAISING   zcx_eam_exception.

    METHODS process_validation
      RAISING zcx_eam_exception.

    DATA gt_staff_assignment      TYPE t_ifcu_pa_station.
    DATA gt_staff_temp_assignment TYPE t_ifcu_pa_sta_tac.
    DATA gt_fuel_consumption_obj  TYPE t_ifcu_tech_obj.
    DATA gf_activity              TYPE activity.
    DATA gt_station_access        TYPE tt_station_access.
    DATA gf_ifcu_process          TYPE ifcu_process.
    DATA gf_temp_access           TYPE flag.

    CONSTANTS gc_auth_sacc     TYPE xuobject VALUE 'ZEAM_S_ASS'.
    CONSTANTS gc_auth_stac     TYPE xuobject VALUE 'ZEAM_T_ASS'.
    CONSTANTS gc_auth_ifcu     TYPE xuobject VALUE 'ZEAM_IFCU'.
    CONSTANTS gc_auth_ifcu_all TYPE xuobject VALUE 'ZIFCU_ALL'.

ENDCLASS.



CLASS ZCL_EAM_IFCU IMPLEMENTATION.


  METHOD constructor.
    " ---------------------------------------------------------------------
    " 10: Assign attributes if provided
    " ---------------------------------------------------------------------
    gf_equnr         = COND #( WHEN equnr IS SUPPLIED THEN equnr ).
    gf_station       = COND #( WHEN station IS SUPPLIED THEN station ).
    gf_posting_date  = COND #( WHEN posting_date IS SUPPLIED THEN posting_date ).
    gf_posting_time  = COND #( WHEN posting_time IS SUPPLIED THEN posting_time ).
    gf_staff_id      = COND #( WHEN staff_id IS SUPPLIED THEN staff_id ).
    gf_short_text    = COND #( WHEN short_text IS SUPPLIED THEN short_text ).
    gf_valid_from    = COND #( WHEN valid_from IS SUPPLIED THEN valid_from ).
    gf_valid_to      = COND #( WHEN valid_to IS SUPPLIED THEN valid_to ).
    gf_ifcu_process  = COND #( WHEN ifcu_process IS SUPPLIED THEN ifcu_process ).
    gf_access_reason = COND #( WHEN access_reason IS SUPPLIED THEN access_reason ).
    gf_attachment    = COND #( WHEN attachment IS SUPPLIED THEN attachment ).
    " ---------------------------------------------------------------------
    " 20: Process validation
    " ---------------------------------------------------------------------
    process_validation( ).
  ENDMETHOD.


  METHOD create_ifcu_consumption.
    TRY.
        " ---------------------------------------------------------------------
        " 10: Create object
        " ---------------------------------------------------------------------
        r_result = NEW #( ).
        " ---------------------------------------------------------------------
        " 20: Assign Required fields
        " ---------------------------------------------------------------------
        r_result->gf_equnr        = equnr.
        r_result->gf_station      = station.
        r_result->gf_posting_date = posting_date.
        r_result->gf_posting_time = posting_time.
        r_result->gf_staff_id     = staff_id.
        r_result->gf_short_text   = short_text.
        r_result->gf_ifcu_process = ifcu_process.
        " ---------------------------------------------------------------------
        " 30: Assign Attachment if provided
        " ---------------------------------------------------------------------
        IF attachment IS SUPPLIED.
          r_result->gf_attachment = attachment.
        ENDIF.
        IF attachment_ttl IS SUPPLIED.
          r_result->gf_attachment_tl = attachment_ttl.
        ENDIF.
        " ---------------------------------------------------------------------
        " 40: Map IFCU process
        " ---------------------------------------------------------------------
        r_result->gs_ifcu_auth = r_result->map_ifcu_process( if_ifcu_process = ifcu_process  ).
        " ---------------------------------------------------------------------
        " 50: Authorization Check
        " ---------------------------------------------------------------------
        r_result->authorization_check( is_ifcu_auth = r_result->gs_ifcu_auth  ).
        " ---------------------------------------------------------------------
        " 60: Process validations
        " ---------------------------------------------------------------------
        r_result->process_validation( ).
        " ---------------------------------------------------------------------
        " 70: Catch exceptions
        " ---------------------------------------------------------------------
      CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
        " ---------------------------------------------------------------------
        " 80: Raise exception
        " ---------------------------------------------------------------------
        RAISE EXCEPTION NEW
          zcx_eam_exception( textid   = zcx_eam_exception=>eam_postingfuelconsump_error
                             previous = lo_cx_eam ).
    ENDTRY.
  ENDMETHOD.


  METHOD create_temp_staff_assignment.
    TRY.
        " ---------------------------------------------------------------------
        " 10: Create object
        " ---------------------------------------------------------------------
        r_result = NEW #( ).
        " ---------------------------------------------------------------------
        " 20: Assign Required fields
        " ---------------------------------------------------------------------
        r_result->gf_station       = station.
        r_result->gf_staff_id      = staff_id.
        r_result->gf_valid_from    = valid_from.
        r_result->gf_valid_to      = valid_to.
        r_result->gf_ifcu_process  = ifcu_process.
        r_result->gf_access_reason = access_reason.
        " ---------------------------------------------------------------------
        " 20.1: Assign Optional fields
        " ---------------------------------------------------------------------
        IF if_temp_access IS SUPPLIED.
          r_result->gf_temp_access = if_temp_access.
        ENDIF.
        " ---------------------------------------------------------------------
        " 30: Map IFCU process
        " ---------------------------------------------------------------------
        r_result->gs_ifcu_auth = r_result->map_ifcu_process( if_ifcu_process = ifcu_process  ).
        " ---------------------------------------------------------------------
        " 40: Authorization Check
        " ---------------------------------------------------------------------
        r_result->authorization_check( is_ifcu_auth = r_result->gs_ifcu_auth  ).
        " ---------------------------------------------------------------------
        " 50: Process validations
        " ---------------------------------------------------------------------
        r_result->process_validation( ).
        " ---------------------------------------------------------------------
        " 60: Catch Exceptions
        " ---------------------------------------------------------------------
      CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
        " ---------------------------------------------------------------------
        " 70: Raise Exceptions
        " ---------------------------------------------------------------------
        RAISE EXCEPTION NEW
          zcx_eam_exception( textid   = zcx_eam_exception=>eam_tempstaffassignment_error
                             previous = lo_cx_eam ).
    ENDTRY.
  ENDMETHOD.


  METHOD create_staff_assignment.
    TRY.
        " ---------------------------------------------------------------------
        " 10: Create object
        " ---------------------------------------------------------------------
        r_result = NEW #( ).
        " ---------------------------------------------------------------------
        " 20: Assign Required fields
        " ---------------------------------------------------------------------
        r_result->gf_station      = station.
        r_result->gf_staff_id     = staff_id.
        r_result->gf_ifcu_process = ifcu_process.
        " ---------------------------------------------------------------------
        " 20.1: Assign Optional fields
        " ---------------------------------------------------------------------
        IF if_temp_access IS SUPPLIED.
          r_result->gf_temp_access = if_temp_access.
        ENDIF.
        " ---------------------------------------------------------------------
        " 30: Map IFCU process
        " ---------------------------------------------------------------------
        r_result->gs_ifcu_auth = r_result->map_ifcu_process( if_ifcu_process = ifcu_process ).
        " ---------------------------------------------------------------------
        " 40: Authorization Check
        " ---------------------------------------------------------------------
        r_result->authorization_check( is_ifcu_auth = r_result->gs_ifcu_auth  ).
        " ---------------------------------------------------------------------
        " 50: Process validations
        " ---------------------------------------------------------------------
        r_result->process_validation( ).
        " ---------------------------------------------------------------------
        " 60: Catch Exceptions
        " ---------------------------------------------------------------------
      CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
        " ---------------------------------------------------------------------
        " 70: Raise Exceptions
        " ---------------------------------------------------------------------
        RAISE EXCEPTION NEW
          zcx_eam_exception( textid   = zcx_eam_exception=>eam_staffassignment_error
                             previous = lo_cx_eam ).
    ENDTRY.
  ENDMETHOD.


  METHOD map_ifcu_process.
    " ---------------------------------------------------------------------
    " 10: Create Activity for IFCU process
    " ---------------------------------------------------------------------
    gs_ifcu_auth = COND #(
    " ---------------------------------------------------------------------
    " 10.1 New Staff Assignment
    " ---------------------------------------------------------------------
        WHEN if_ifcu_process = gc_new_staff_station_access    THEN VALUE #( ifcu_proc = gc_proc_sacc
                                                                            ifcu_actv = gc_create )
    " ---------------------------------------------------------------------
    " 10.2 Revoke Staff Assignment
    " ---------------------------------------------------------------------
        WHEN if_ifcu_process = gc_revoke_staff_station_access THEN VALUE #( ifcu_proc = gc_proc_sacc
                                                                            ifcu_actv = gc_revoke )
    " ---------------------------------------------------------------------
    " 10.3 New Temporary Staff Assignment
    " ---------------------------------------------------------------------
        WHEN if_ifcu_process = gc_new_temp_staff_station_acc  THEN VALUE #( ifcu_proc = gc_proc_stac
                                                                            ifcu_actv = gc_create )
    " ---------------------------------------------------------------------
    " 10.4 Revoke Temporary Staff Assignment
    " ---------------------------------------------------------------------
        WHEN if_ifcu_process = gc_revoke_temp_staff_stat_acc  THEN VALUE #( ifcu_proc = gc_proc_stac
                                                                            ifcu_actv = gc_revoke )
    " ---------------------------------------------------------------------
    " 10.5 Get Staff Assignment
    " ---------------------------------------------------------------------
        WHEN if_ifcu_process = gc_get_staff_access            THEN VALUE #( ifcu_proc = gc_proc_sacc
                                                                            ifcu_actv = gc_display )
    " ---------------------------------------------------------------------
    " 10.6 Get Temporary Staff Assignment
    " ---------------------------------------------------------------------
        WHEN if_ifcu_process = gc_get_temp_staff_access       THEN VALUE #( ifcu_proc = gc_proc_stac
                                                                            ifcu_actv = gc_display )
    " ---------------------------------------------------------------------
    " 10.7 New Fuel Consumption
    " ---------------------------------------------------------------------
        WHEN if_ifcu_process = gc_new_fuel_consumption        THEN VALUE #( ifcu_proc = gc_proc_ifcu
                                                                            ifcu_actv = gc_create )
    " ---------------------------------------------------------------------
    " 10.8 Get Fuel Consumption
    " ---------------------------------------------------------------------
        WHEN if_ifcu_process = gc_get_fuel_consumption        THEN VALUE #( ifcu_proc = gc_proc_ifcu
                                                                            ifcu_actv = gc_display ) ).
    rs_ifcu_auth = gs_ifcu_auth.
    " ---------------------------------------------------------------------
    " 20: Raise Exception if process does not exist
    " ---------------------------------------------------------------------
    IF rs_ifcu_auth IS INITIAL.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid = zcx_eam_exception=>eam_ifcuprocessdoesnotexist ).
    ENDIF.
  ENDMETHOD.


  METHOD authorization_check.
    " ---------------------------------------------------------------------
    " 10: Unset Granted Access
    " ---------------------------------------------------------------------
    rf_granted = abap_false.
    " ---------------------------------------------------------------------
    " 20: Check IFCU process
    " ---------------------------------------------------------------------
    CASE is_ifcu_auth-ifcu_proc.
      " ---------------------------------------------------------------------
      " 30: IFCU process: Staff Assignment
      " ---------------------------------------------------------------------
      WHEN gc_proc_sacc.
        " ---------------------------------------------------------------------
        " 30.1: Auth Check?
        " ---------------------------------------------------------------------
        AUTHORITY-CHECK OBJECT gc_auth_sacc
                        ID 'ACTVT' FIELD is_ifcu_auth-ifcu_actv.
        " ---------------------------------------------------------------------
        " 30.2: Auth. failed?
        " ---------------------------------------------------------------------
        IF xsdbool( sy-subrc IS INITIAL )
           = abap_false.
          " ---------------------------------------------------------------------
          " 30.3: Additional Auth. Check Required?
          " ---------------------------------------------------------------------
          AUTHORITY-CHECK OBJECT gc_auth_ifcu_all
                          ID 'ZIFCU_ACTV' FIELD gc_proc_sacc
                          ID 'ACTVT' FIELD is_ifcu_auth-ifcu_actv.
          " ---------------------------------------------------------------------
          " 30.4: Raise Exception if Auth. Check fails
          " ---------------------------------------------------------------------
          IF xsdbool( sy-subrc IS INITIAL )
             = abap_false.
            RAISE EXCEPTION NEW
              zcx_eam_exception( textid       = zcx_eam_exception=>eam_missingauthorization
                                 eam_auth_txt = COND #( WHEN is_ifcu_auth-ifcu_actv
                                                             = gc_create            THEN gc_auth_error_sacc_01
                                                        WHEN is_ifcu_auth-ifcu_actv
                                                             = gc_display           THEN gc_auth_error_sacc_03
                                                        WHEN is_ifcu_auth-ifcu_actv
                                                             = gc_revoke            THEN gc_auth_error_sacc_06 ) ).
          ENDIF.
        ENDIF.
        " ---------------------------------------------------------------------
        " 40: IFCU process: Staff temporary Assignment
        " ---------------------------------------------------------------------
      WHEN gc_proc_stac.
        " ---------------------------------------------------------------------
        " 40.1: Auth Check?
        " ---------------------------------------------------------------------
        AUTHORITY-CHECK OBJECT gc_auth_stac
                        ID 'ACTVT' FIELD is_ifcu_auth-ifcu_actv.
        " ---------------------------------------------------------------------
        " 40.2: Auth. failed?
        " ---------------------------------------------------------------------
        IF xsdbool( sy-subrc IS INITIAL )
           = abap_false.
          " ---------------------------------------------------------------------
          " 40.3: Additional Auth. Check Required?
          " ---------------------------------------------------------------------
          AUTHORITY-CHECK OBJECT gc_auth_ifcu_all
                          ID 'ZIFCU_ACTV' FIELD gc_proc_stac
                          ID 'ACTVT' FIELD is_ifcu_auth-ifcu_actv.
          " ---------------------------------------------------------------------
          " 40.4: Raise Exception if Auth. Check fails
          " ---------------------------------------------------------------------
          IF xsdbool( sy-subrc IS INITIAL )
             = abap_false.
            RAISE EXCEPTION NEW
              zcx_eam_exception( textid       = zcx_eam_exception=>eam_missingauthorization
                                 eam_auth_txt = COND #( WHEN is_ifcu_auth-ifcu_actv
                                                             = gc_create            THEN gc_auth_error_stac_01
                                                        WHEN is_ifcu_auth-ifcu_actv
                                                             = gc_display           THEN gc_auth_error_stac_03
                                                        WHEN is_ifcu_auth-ifcu_actv
                                                             = gc_revoke            THEN gc_auth_error_stac_06 ) ).
          ENDIF.
        ENDIF.
        " ---------------------------------------------------------------------
        " 50: IFCU process: Fuel Consumption
        " ---------------------------------------------------------------------
      WHEN gc_proc_ifcu.
        " ---------------------------------------------------------------------
        " 50.1: Auth Check?
        " ---------------------------------------------------------------------
        AUTHORITY-CHECK OBJECT gc_auth_ifcu
                        ID 'ACTVT' FIELD is_ifcu_auth-ifcu_actv.
        " ---------------------------------------------------------------------
        " 50.2: Auth. failed?
        " ---------------------------------------------------------------------
        IF xsdbool( sy-subrc IS INITIAL )
           = abap_false.
          " ---------------------------------------------------------------------
          " 50.3: Additional Auth. Check Required?
          " ---------------------------------------------------------------------
          AUTHORITY-CHECK OBJECT gc_auth_ifcu_all
                          ID 'ZIFCU_ACTV' FIELD gc_proc_ifcu
                          ID 'ACTVT' FIELD is_ifcu_auth-ifcu_actv.
          " ---------------------------------------------------------------------
          " 50.4: Raise Exception if Auth. Check fails
          " ---------------------------------------------------------------------
          IF xsdbool( sy-subrc IS INITIAL )
             = abap_false.
            RAISE EXCEPTION NEW
              zcx_eam_exception( textid       = zcx_eam_exception=>eam_missingauthorization
                                 eam_auth_txt = COND #( WHEN is_ifcu_auth-ifcu_actv
                                                             = gc_create            THEN gc_auth_error_ifcu_01
                                                        WHEN is_ifcu_auth-ifcu_actv
                                                             = gc_display           THEN gc_auth_error_ifcu_03 ) ).
          ENDIF.
        ENDIF.
    ENDCASE.
    " ---------------------------------------------------------------------
    " 60: Set Granted Access
    " ---------------------------------------------------------------------
    rf_granted = abap_true.
  ENDMETHOD.


  METHOD process_validation.
    " ---------------------------------------------------------------------
    " 10: Subscribe: field validations
    " ---------------------------------------------------------------------
    zcl_eam_ifcu_gen=>process_field_validation( me ).
  ENDMETHOD.


  METHOD get_staff_access.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    data_declaration.
    " ---------------------------------------------------------------------
    " 20: Assign selection fields
    " ---------------------------------------------------------------------
    lr_staff_id         = COND #( WHEN ir_staff_id IS SUPPLIED
                                  THEN ir_staff_id ).
    lr_staff_first_name = COND #( WHEN ir_staff_first_name IS SUPPLIED
                                  THEN ir_staff_first_name ).
    lr_staff_last_name  = COND #( WHEN ir_staff_last_name IS SUPPLIED
                                  THEN ir_staff_last_name ).
    lr_station          = COND #( WHEN ir_station IS SUPPLIED
                                  THEN ir_station ).
    lr_station_t        = COND #( WHEN ir_station_t IS SUPPLIED
                                  THEN ir_station_t ).
    lr_valid_from       = COND #( WHEN ir_valid_from IS SUPPLIED
                                  THEN ir_valid_from ).
    lr_valid_to         = COND #( WHEN ir_valid_to IS SUPPLIED
                                  THEN ir_valid_to ).
    authorization_check( is_ifcu_auth = gs_ifcu_auth  ).
*    CATCH zcx_eam_exception. " EAM Exception Class
    " ---------------------------------------------------------------------
    " 30: Query: Get station access
    " ---------------------------------------------------------------------
    SELECT
      FROM zifcu_pa_station AS pa_access
             INNER JOIN
               ( pa0002 AS pa_info
                   LEFT OUTER JOIN
                     pa0105 AS pa_comm ON  pa_comm~pernr  = pa_info~pernr
                                       AND pa_comm~endda >= @sy-datum
                                       AND pa_comm~usrty  = '0001' ) ON  pa_info~pernr
                                                                         = pa_access~pernr
                                                                     AND pa_info~endda
                                                                         >= @sy-datum
                 INNER JOIN
                   (
                       t370fld_stn AS gas_station
                         INNER JOIN
                           t370fld_stn_t AS station_text ON  gas_station~station
                                                             = station_text~station
                                                         AND lang_key
                                                             = 'E' ) ON pa_access~station
                                                                        = gas_station~station
                     LEFT OUTER JOIN
                       ( zifcu_statn_loc AS statn_loc
                           INNER JOIN
                             zifcu_locations AS locations ON statn_loc~loc_code = locations~loc_code ) ON pa_access~station = statn_loc~station
      FIELDS pa_access~pernr        AS staff_id,
             pa_info~vorna          AS first_name,
             pa_info~nachn          AS last_name,
             pa_comm~usrid          AS usnam,
             gas_station~station    AS station,
             gas_station~station_t  AS station_t,
             station_text~type_text AS station_name,
             locations~loc_name     AS loc_name,
             @abap_false            AS temporary_access,
             @lf_nd                 AS valid_from,
             @lf_nd                 AS valid_to

      WHERE pa_access~pernr        IN @lr_staff_id
        AND pa_info~vorna          IN @lr_staff_first_name
        AND pa_info~nachn          IN @lr_staff_last_name
        AND pa_access~station      IN @lr_station
        AND gas_station~station_t  IN @lr_station_t
        AND station_text~type_text IN @lr_station_name

    UNION ALL

    SELECT
      FROM zifcu_pa_sta_tac AS pa_t_access
             INNER JOIN
               ( pa0002 AS pa_info
                   LEFT OUTER JOIN
                     pa0105 AS pa_comm ON  pa_comm~pernr  = pa_info~pernr
                                       AND pa_comm~endda >= @sy-datum
                                       AND pa_comm~usrty  = '0001' ) ON  pa_info~pernr
                                                                         = pa_t_access~pernr
                                                                     AND pa_info~endda
                                                                         >= @sy-datum
                 INNER JOIN
                   (
                       t370fld_stn AS gas_station
                         INNER JOIN
                           t370fld_stn_t AS station_text ON  gas_station~station
                                                             = station_text~station
                                                         AND lang_key
                                                             = 'E' ) ON pa_t_access~station
                                                                        = gas_station~station
                     LEFT OUTER JOIN
                       ( zifcu_statn_loc AS statn_loc
                           INNER JOIN
                             zifcu_locations AS locations ON statn_loc~loc_code = locations~loc_code ) ON pa_t_access~station = statn_loc~station

      FIELDS pa_t_access~pernr      AS staff_id,
             pa_info~vorna          AS first_name,
             pa_info~nachn          AS last_name,
             pa_comm~usrid          AS usnam,
             gas_station~station    AS station,
             gas_station~station_t  AS station_t,
             station_text~type_text AS station_name,
             locations~loc_name     AS loc_name,
             @abap_true             AS temporary_access,
             pa_t_access~valid_from AS valid_from,
             pa_t_access~valid_to   AS valid_to

      WHERE pa_t_access~pernr      IN @lr_staff_id
        AND pa_info~vorna          IN @lr_staff_first_name
        AND pa_info~nachn          IN @lr_staff_last_name
        AND pa_t_access~station    IN @lr_station
        AND gas_station~station_t  IN @lr_station_t
        AND station_text~type_text IN @lr_station_name
        AND pa_t_access~valid_from IN @lr_valid_from
        AND pa_t_access~valid_to   IN @lr_valid_to
        and pa_t_access~valid_to >= @sy-datum

    INTO CORRESPONDING FIELDS OF TABLE @rt_staff_access.

    IF if_include_temp_access = abap_false.
      DELETE rt_staff_access WHERE temporary_access = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD create_sacc.
    " ---------------------------------------------------------------------
    " 10: Data creation
    " ---------------------------------------------------------------------
    DATA(ls_sacc) = VALUE zifcu_pa_station( pernr   = gf_staff_id
                                            station = gf_station ).
    " ---------------------------------------------------------------------
    " 20: Update Etag
    " ---------------------------------------------------------------------
    gs_etag = zcl_eam_ifcu_gen=>update_etag( me ).
    ls_sacc-etag = CORRESPONDING #( gs_etag ).

    SELECT SINGLE FROM zifcu_pa_station
      FIELDS @abap_true
      WHERE station = @gf_station
        AND pernr   = @gf_staff_id
      INTO @DATA(lf_assignment).

    IF lf_assignment = abap_true.
      RAISE EXCEPTION NEW zcx_eam_exception( textid      = zcx_eam_exception=>eam_staff_alreadyassigned
                                             eam_staff   = CONV #( gf_staff_id )
                                             eam_station = zcl_eam_ifcu_gen=>retrieve__gas_station( gf_station ) ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 30: Create Record
    " ---------------------------------------------------------------------
    INSERT zifcu_pa_station
    FROM ls_sacc.
    " ---------------------------------------------------------------------
    " 40: Raise exception
    " ---------------------------------------------------------------------
    IF sy-subrc = 0.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid      = zcx_eam_exception=>eam_staffassignedtostation
                           eam_staff   = CONV #( gf_staff_id )
                           eam_station = zcl_eam_ifcu_gen=>retrieve__gas_station( gf_station ) ).

    ENDIF.
  ENDMETHOD.


  METHOD create_stac.
    " ---------------------------------------------------------------------
    " 10: Data creation
    " ---------------------------------------------------------------------
    DATA(ls_stac) = VALUE zifcu_pa_sta_tac( pernr         = gf_staff_id
                                            station       = gf_station
                                            valid_from    = gf_valid_from
                                            valid_to      = gf_valid_to
                                            access_reason = gf_access_reason
                                            status        = abap_true ).
    " ---------------------------------------------------------------------
    " 20: Update Etag
    " ---------------------------------------------------------------------
    gs_etag = zcl_eam_ifcu_gen=>update_etag( me ).
    ls_stac-etag = CORRESPONDING #( gs_etag ).
    " ---------------------------------------------------------------------
    " 30: Create Record
    " ---------------------------------------------------------------------
    INSERT zifcu_pa_sta_tac
    FROM ls_stac.
    " ---------------------------------------------------------------------
    " 40: Raise exception
    " ---------------------------------------------------------------------
    IF sy-subrc = 0.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid      = zcx_eam_exception=>eam_staffassignedtempstation
                           eam_staff   = CONV #( gf_staff_id )
                           eam_station = zcl_eam_ifcu_gen=>retrieve__gas_station( gf_station ) ).

    ENDIF.
  ENDMETHOD.


  METHOD revoke_sacc.
    " ---------------------------------------------------------------------
    " 10: Data creation
    " ---------------------------------------------------------------------
    DATA(ls_sacc) = VALUE zifcu_pa_station( pernr   = gf_staff_id
                                            station = gf_station ).
    " ---------------------------------------------------------------------
    " 20: Update Etag
    " ---------------------------------------------------------------------
    gs_etag = zcl_eam_ifcu_gen=>update_etag( me ).
    ls_sacc-etag = CORRESPONDING #( gs_etag ).

    " ---------------------------------------------------------------------
    " 30: Check access exists
    " ---------------------------------------------------------------------
    IF gf_temp_access = abap_false.
      SELECT SINGLE FROM zifcu_pa_station
        FIELDS @abap_true
        WHERE station = @gf_station
          AND pernr   = @gf_staff_id
        INTO @DATA(lf_assignment).
    ELSE.
      SELECT SINGLE FROM zifcu_pa_sta_tac
        FIELDS @abap_true
        WHERE station    = @gf_station
          AND pernr      = @gf_staff_id
          AND valid_from = @gf_valid_from
          AND valid_to   = @gf_valid_to
        INTO @lf_assignment.
    ENDIF.
    " ---------------------------------------------------------------------
    " 40: Delete Record
    " ---------------------------------------------------------------------
    IF lf_assignment = abap_true.
      IF gf_temp_access = abap_false.
        DELETE FROM zifcu_pa_station
        WHERE pernr   = @ls_sacc-pernr
          AND station = @ls_sacc-station.
      ELSE.
        DELETE FROM zifcu_pa_sta_tac
        WHERE pernr      = @ls_sacc-pernr
          AND station    = @ls_sacc-station
          AND valid_from = @gf_valid_from
          AND valid_to   = @gf_valid_to.
      ENDIF.
    ENDIF.
    " ---------------------------------------------------------------------
    " 50: Raise exception
    " ---------------------------------------------------------------------
    IF sy-subrc = 0.
      RAISE EXCEPTION NEW
        zcx_eam_exception( textid      = zcx_eam_exception=>eam_stationass_withdrawn
                           eam_staff   = CONV #( gf_staff_id )
                           eam_station = zcl_eam_ifcu_gen=>retrieve__gas_station( gf_station ) ).

    ENDIF.
  ENDMETHOD.


  METHOD create_ifcu.
*    BREAK csayeh.
    " ---------------------------------------------------------------------
    " 10: Data creation
    " ---------------------------------------------------------------------
    DATA(ls_ifcu) = VALUE zifcu_tech_obj( equnr           = gf_equnr
                                          station         = gf_station
                                          posting_date    = gf_posting_date
                                          posting_time    = gf_posting_time
                                          pernr           = if_staff_id
                                          attendant_pernr = gf_staff_id
                                          short_text      = gf_short_text
                                          doc_description = gf_attachment_tl
*                                          dokar           =
*                                          doknr           =
*                                          dokvr           =
*                                          doktl           =
    ).
    " ---------------------------------------------------------------------
    " 20: Update Etag
    " ---------------------------------------------------------------------
    gs_etag = zcl_eam_ifcu_gen=>update_etag( me ).
    ls_ifcu-etag = CORRESPONDING #( gs_etag ).
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " 30: Check if attachment already exists
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE
      FROM zifcu_tech_obj AS tech_obj
             INNER JOIN
               draw ON  tech_obj~dokar
                        = draw~dokar
                    AND tech_obj~doknr
                        = draw~doknr
                    AND tech_obj~dokvr
                        = draw~dokvr
                    AND tech_obj~doktl
                        = draw~doktl
      FIELDS @abap_true,
             draw~dokar,
             draw~doknr,
             draw~dokvr,
             draw~doktl
      WHERE equnr =
            @gf_equnr
        AND station      = @gf_station
        AND posting_date =
            @gf_posting_date
        AND posting_time =
            @gf_posting_time
        AND pernr = @if_staff_id
      INTO ( @DATA(lf_document_exists),
             @ls_ifcu-dokar,
             @ls_ifcu-doknr,
             @ls_ifcu-dokvr,
             @ls_ifcu-doktl ).
    " ---------------------------------------------------------------------
    " 40: If attachment exists only modify records
    " ---------------------------------------------------------------------
    IF lf_document_exists = abap_true.
      MODIFY
      zifcu_tech_obj
      FROM
      ls_ifcu.
      " ---------------------------------------------------------------------
      " 50: Raise exception If records update fails
      " ---------------------------------------------------------------------
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW
          zcx_eam_exception( textid = zcx_eam_exception=>eam_postingfuelconsump_error ).
      ENDIF.
    ELSE.
      " ---------------------------------------------------------------------
      " 60: IF attachment is provided upload file
      " ---------------------------------------------------------------------
      IF gf_attachment IS NOT INITIAL.
        TRY.
            DATA(ls_draw) = zcl_eam_ifcu_gen=>eam_file_upload( io_eam_ifcu = me ).
            IF ls_draw IS NOT INITIAL.
              ls_ifcu = CORRESPONDING #( BASE ( ls_ifcu ) ls_draw ).
            ENDIF.
            " ---------------------------------------------------------------------
            " 70: Error uploading attachment
            " ---------------------------------------------------------------------
          CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
            RAISE EXCEPTION NEW zcx_eam_exception( textid   = zcx_eam_exception=>eam_erroruploadingattachment
                                                   previous = lo_cx_eam ).
        ENDTRY.
        " ---------------------------------------------------------------------
        " 80: If document uploaded
        " ---------------------------------------------------------------------
        IF ls_draw-doknr IS NOT INITIAL.
          " ---------------------------------------------------------------------
          " 90: Modify records
          " ---------------------------------------------------------------------
          MODIFY
            zifcu_tech_obj
            FROM
            ls_ifcu.
          " ---------------------------------------------------------------------
          " 100: Raise exception if update fails
          " ---------------------------------------------------------------------
          IF sy-subrc <> 0.
            RAISE EXCEPTION NEW
              zcx_eam_exception( textid = zcx_eam_exception=>eam_postingfuelconsump_error ).
            " ---------------------------------------------------------------------
            " 100: Successful exception: Uploaded
            " ---------------------------------------------------------------------
          ELSE.
            RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_doc_upload_success ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 110: Raise exception If no document number was returned
          " ---------------------------------------------------------------------
        ELSE.
          RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_erroruploadingattachment ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 80 No attachment provided
        " ---------------------------------------------------------------------
      ELSE.
        " ---------------------------------------------------------------------
        " 90: Modify records
        " ---------------------------------------------------------------------
        MODIFY
          zifcu_tech_obj
          FROM
          ls_ifcu.
        " ---------------------------------------------------------------------
        " 100: Raise exception If record update fails.
        " ---------------------------------------------------------------------
        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW
            zcx_eam_exception(
              textid = zcx_eam_exception=>eam_postingfuelconsump_error ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
