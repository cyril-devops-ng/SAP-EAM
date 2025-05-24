*&---------------------------------------------------------------------*
*& Include          ZEAM_IFCU_STAFF_REPORT_LCL
*&---------------------------------------------------------------------*
CLASS lcl_ifcu_staff_report IMPLEMENTATION.
  METHOD create_report.
    TRY.
        " ---------------------------------------------------------------------
        " 10: Create new instance
        " ---------------------------------------------------------------------
        r_result = NEW #( if_equnr        = if_equnr
                          if_station      = if_station
                          if_posting_date = if_posting_date ).
        " ---------------------------------------------------------------------
        " 20: Check required criteria
        " ---------------------------------------------------------------------
        IF xsdbool(
            ( ir_equnr IS NOT SUPPLIED
              AND ir_station_t IS NOT SUPPLIED
              AND ir_posting_date IS NOT SUPPLIED
              AND ir_collector_id IS NOT SUPPLIED
              AND ir_attendant_id IS NOT SUPPLIED )
              OR ( ir_equnr IS INITIAL
                AND ir_station_t IS INITIAL
                AND ir_posting_date IS INITIAL
                AND ir_collector_id IS INITIAL
                AND ir_attendant_id IS INITIAL )
              AND
              p_m_read = abap_true )
           = abap_true.
          " ---------------------------------------------------------------------
          " 30: Raise exception
          " ---------------------------------------------------------------------
          RAISE EXCEPTION NEW zcx_eam_exception( textid    = zcx_eam_exception=>eam_gen_error
                                                 eam_error = gc_mult_rec_error ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 40: Assign attributes
        " ---------------------------------------------------------------------
        IF p_m_read = abap_true.
          r_result->lr_requnr       = COND #( WHEN ir_equnr IS SUPPLIED THEN ir_equnr ).
          r_result->lr_station_t    = COND #( WHEN ir_station_t IS SUPPLIED THEN ir_station_t ).
          r_result->lr_posting_date = COND #( WHEN ir_posting_date IS SUPPLIED THEN ir_posting_date ).
          r_result->lr_collector_id = COND #( WHEN ir_collector_id IS SUPPLIED THEN ir_collector_id ).
          r_result->lr_attendant_id = COND #( WHEN ir_attendant_id IS SUPPLIED THEN ir_attendant_id ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 50: Retrieve set
        " ---------------------------------------------------------------------
        r_result->retrieve_set( ).
        " ---------------------------------------------------------------------
        " 50: Raise class exception
        " ---------------------------------------------------------------------
      CATCH zcx_eam_exception INTO DATA(lo_cx_eam).
        RAISE EXCEPTION NEW zcx_eam_exception( textid   = zcx_eam_exception=>eam_report_generation
                                               previous = lo_cx_eam ).
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    " ---------------------------------------------------------------------
    " 10: Assign equipment no.
    " ---------------------------------------------------------------------
    IF xsdbool( if_equnr IS SUPPLIED AND if_equnr IS NOT INITIAL ) = abap_true.
      INSERT VALUE #( low    = if_equnr
                      sign   = 'I'
                      option = 'EQ' )
             INTO TABLE lr_requnr.
    ENDIF.
    " ---------------------------------------------------------------------
    " 20: Assign station
    " ---------------------------------------------------------------------
    IF xsdbool( if_station IS SUPPLIED AND if_station IS NOT INITIAL ) = abap_true.
      INSERT VALUE #( low    = if_station
                      sign   = 'I'
                      option = 'EQ' )
             INTO TABLE lr_station_t.
    ENDIF.
    " ---------------------------------------------------------------------
    " 30: Assign posting date
    " ---------------------------------------------------------------------
    IF xsdbool( if_posting_date IS SUPPLIED AND if_posting_date IS NOT INITIAL ) = abap_true.
      INSERT VALUE #( low    = if_posting_date
                      sign   = 'I'
                      option = 'EQ' )
             INTO TABLE lr_posting_date.
    ENDIF.
    " ---------------------------------------------------------------------
    " 40: Check required criteria
    " ---------------------------------------------------------------------
    IF xsdbool( ( if_equnr IS NOT SUPPLIED
                AND if_station IS NOT SUPPLIED
                AND if_posting_date IS NOT SUPPLIED )
                OR (
                if_equnr IS INITIAL
                AND if_station IS INITIAL
                AND if_posting_date IS INITIAL )

                AND p_s_read = abap_true )
       = abap_true.
      " ---------------------------------------------------------------------
      " 50: Raise exception
      " ---------------------------------------------------------------------
      RAISE EXCEPTION NEW zcx_eam_exception( textid    = zcx_eam_exception=>eam_gen_error
                                             eam_error = gc_sing_rec_error ).

    ENDIF.
  ENDMETHOD.

  METHOD create_grid.
    " ---------------------------------------------------------------------
    " 10: Create ALV
    " ---------------------------------------------------------------------
    TRY.
        IF go_alv IS INITIAL.
          cl_salv_table=>factory( EXPORTING r_container  = cl_gui_container=>default_screen
                                  IMPORTING r_salv_table = go_alv
                                  CHANGING  t_table      = ct_tab ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 20: Raise Exception
        " ---------------------------------------------------------------------
      CATCH cx_salv_msg INTO DATA(lo_alv_ex).
        RAISE EXCEPTION NEW zcx_eam_exception( textid   = zcx_eam_exception=>eam_alv_error
                                               previous = lo_alv_ex ).
    ENDTRY.
    " ---------------------------------------------------------------------
    " 40: Set up ALv
    " ---------------------------------------------------------------------
    IF go_alv IS BOUND.
      TRY.
          go_alv->get_functions( )->set_all( abap_true ).

          SET HANDLER on_added_function FOR go_alv->get_event( ).
          SET HANDLER on_link_click     FOR go_alv->get_event( ).
          " ---------------------------------------------------------------------
          " 50: Raise Exception
          " ---------------------------------------------------------------------
        CATCH cx_salv_existing
              cx_salv_wrong_call
              cx_salv_method_not_supported.
          RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_alv_error ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD on_added_function.
    CASE e_salv_function.
      " ---------------------------------------------------------------------
      " 50: Refresh func.
      " ---------------------------------------------------------------------
      WHEN gc_refresh.
        refresh_grid( ).
    ENDCASE.
  ENDMETHOD.

  METHOD on_link_click.
    " ---------------------------------------------------------------------
    " 10: Check record exists
    " ---------------------------------------------------------------------
    IF NOT line_exists( gt_output[ row ] ).
      RETURN.
    ENDIF.

    ASSIGN gt_output[ row ] TO FIELD-SYMBOL(<fs>).
    CASE column.
      " ---------------------------------------------------------------------
      " 20: Show attachment
      " ---------------------------------------------------------------------
      WHEN 'ATTACHMENT'.
        TRY.
            TRY.
                show_attachment( is_output = <fs>  ).
              CATCH zcx_eam_exception INTO DATA(lo_doc_error).
                RAISE EXCEPTION NEW zcx_eam_exception( textid   = zcx_eam_exception=>eam_doc_error
                                                       previous = lo_doc_error ).
            ENDTRY.
          CATCH zcx_eam_exception INTO DATA(lo_eam_att_ex).
            DATA(lt_msg) = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_eam_att_ex ).
            IF line_exists( lt_msg[ number = '042' ] ).
              ASSIGN lt_msg[ number = '042' ] TO FIELD-SYMBOL(<fs_doc_msg>).
              <fs_doc_msg>-type = 'S'.
              DELETE lt_msg WHERE number = '040'.
            ENDIF.
            zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
        ENDTRY.
        " ---------------------------------------------------------------------
        " 30: Show remarks
        " ---------------------------------------------------------------------
      WHEN 'REMARKS'.
        TRY.
            show_remarks( is_output = <fs>  ).
          CATCH zcx_eam_exception INTO DATA(lo_eam_rem_ex).
            lt_msg = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_eam_rem_ex ).
            zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
        ENDTRY.
        " ---------------------------------------------------------------------
        " 40: Show Material Document
        " ---------------------------------------------------------------------
      WHEN 'MBLNR' OR 'MJAHR'.
        SELECT SINGLE FROM mkpf
          FIELDS @abap_true
          WHERE mblnr
                = @<fs>-mblnr
            AND mjahr
                = @<fs>-mjahr
          INTO @DATA(lf_mat_doc_exists).
        " ---------------------------------------------------------------------
        " 40.1: Check Material document exists
        " ---------------------------------------------------------------------
        IF lf_mat_doc_exists = abap_false.
          RETURN.
        ENDIF.
        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING i_action            = 'A04'
                    i_refdoc            = 'R02'
                    i_notree            = 'X'
                    i_no_auth_check     = ' '
                    i_deadend           = 'X'
                    i_skip_first_screen = 'X'
                    i_okcode            = 'OK_GO'
                    i_mblnr             = <fs>-mblnr
                    i_mjahr             = <fs>-mjahr.

    ENDCASE.
  ENDMETHOD.

  METHOD refresh_grid.
    " ---------------------------------------------------------------------
    " 10: Unset report data
    " ---------------------------------------------------------------------
    CLEAR gt_output.
    TRY.
        " ---------------------------------------------------------------------
        " 20: Retrieve report data
        " ---------------------------------------------------------------------
        retrieve_set( ).
        " ---------------------------------------------------------------------
        " 30: Handle exception
        " ---------------------------------------------------------------------
      CATCH zcx_eam_exception INTO DATA(lo_eam_ex).
        DATA(lt_msg) = zcl_eam_ifcu_gen=>create_bapi_ret_from_exception( lo_eam_ex ).
        zcl_eam_ifcu_gen=>display_bapi_log_gui( lt_msg ).
    ENDTRY.
    " ---------------------------------------------------------------------
    " 40: GUI Refresh
    " ---------------------------------------------------------------------
    go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD retrieve_set.
    " ---------------------------------------------------------------------
    " 10: SQL Query for report
    " ---------------------------------------------------------------------
    SELECT
    " ---------------------------------------------------------------------
    " 20: Fleet Data
    " ---------------------------------------------------------------------
      FROM ( fleet AS f
               INNER JOIN
                 equi AS eq ON f~objnr = eq~objnr
                   INNER JOIN
                     eam_ifcu AS ifcu ON eq~objnr =
                                         ifcu~objnr
                       INNER JOIN
    " ---------------------------------------------------------------------
    " 30: Material Document Data
    " ---------------------------------------------------------------------
                         mkpf AS mk ON  ifcu~mblnr =
                                        mk~mblnr
                                    AND ifcu~mjahr = mk~mjahr
                           INNER JOIN
                             mseg AS ms ON  mk~mblnr = ms~mblnr
                                        AND
                                            mk~mjahr = ms~mjahr )
        " ---------------------------------------------------------------------
        " 30.1 Measurement in hours
        " ---------------------------------------------------------------------
             LEFT OUTER JOIN
               ( imptt AS mpoint_hr
                   INNER JOIN
                     imrg AS mdoc_hr ON mpoint_hr~point = mdoc_hr~point ) ON  eq~objnr = mpoint_hr~mpobj
                                                                          AND ( mpoint_hr~mrngu = 'HR' OR mpoint_hr~mrngu = 'H' OR mpoint_hr~mrngu = 'STD' )
                                                                          AND ifcu~posting_date = mdoc_hr~idate
                                                                          AND ifcu~posting_time = mdoc_hr~itime
        " ---------------------------------------------------------------------
        " 30.2 Measurement in kilometers
        " ---------------------------------------------------------------------
                 LEFT OUTER JOIN
                   ( imptt AS mpoint_km
                       INNER JOIN
                         imrg AS mdoc_km ON mpoint_km~point = mdoc_km~point ) ON  eq~objnr = mpoint_km~mpobj
                                                                              AND ( mpoint_km~mrngu = 'KM' )
                                                                              AND ifcu~posting_date = mdoc_km~idate
                                                                              AND ifcu~posting_time = mdoc_km~itime
        " ---------------------------------------------------------------------
        " 30.3 Measurement in Litres
        " ---------------------------------------------------------------------
                     LEFT OUTER JOIN
                       ( imptt AS mpoint_l
                           INNER JOIN
                             imrg AS mdoc_l ON mpoint_l~point = mdoc_l~point ) ON  eq~objnr = mpoint_l~mpobj
                                                                               AND ( mpoint_l~mrngu = 'L' OR mpoint_l~mrngu = 'l' )
                                                                               AND ifcu~posting_date = mdoc_l~idate
                                                                               AND ifcu~posting_time = mdoc_l~itime
        " ---------------------------------------------------------------------
        " 30.4 Measurement in degree celsius
        " ---------------------------------------------------------------------
                         LEFT OUTER JOIN
                           ( imptt AS mpoint_temp
                               INNER JOIN
                                 imrg AS mdoc_temp ON mpoint_temp~point = mdoc_temp~point ) ON  eq~objnr = mpoint_temp~mpobj
                                                                                            AND ( mpoint_temp~mrngu = 'GC' )
                                                                                            AND ifcu~posting_date = mdoc_temp~idate
                                                                                            AND ifcu~posting_time = mdoc_temp~itime
        " ---------------------------------------------------------------------
        " 30.5 Measurement in litre per 100km
        " ---------------------------------------------------------------------
                             LEFT OUTER JOIN
                               ( imptt AS mpoint_l100
                                   INNER JOIN
                                     imrg AS mdoc_l100 ON mpoint_l100~point = mdoc_l100~point ) ON  eq~objnr = mpoint_l100~mpobj
                                                                                                AND ( mpoint_l100~mrngu = 'LHK' )
                                                                                                AND ifcu~posting_date = mdoc_l100~idate
                                                                                                AND ifcu~posting_time = mdoc_l100~itime

                                 INNER JOIN
                                   (
                        " ---------------------------------------------------------------------
                        " 40: Station Data
                        " ---------------------------------------------------------------------
                    t370fld_stn AS stn
                      INNER JOIN
                        t370fld_stn_t AS stn_t ON stn~station
                                                  = stn_t~station ) ON  ms~werks = stn~plant
                                                                    AND ms~lgort =
                                                                        stn~storage
                                     INNER JOIN
    " ---------------------------------------------------------------------
    " 50: Material Data
    " ---------------------------------------------------------------------
                                       t370fld_mat AS t_mat ON  ms~matnr    = t_mat~matnr
                                                            AND stn~station = t_mat~station
                                         LEFT OUTER JOIN
                                           (
                        " ---------------------------------------------------------------------
                        " 60: Custom Data
                        " ---------------------------------------------------------------------
                        zifcu_tech_obj AS zifcu
                          INNER JOIN
                        " ---------------------------------------------------------------------
                        " 70: Employee Data (Attendant & Driver )
                        " ---------------------------------------------------------------------
                            pa0002 AS collector ON  zifcu~pernr      = collector~pernr
                                                AND collector~endda >= @sy-datum
                              INNER JOIN
                                ( pa0002 AS attendant
                                    LEFT OUTER JOIN
                                      pa0105 AS attendant_comm ON  attendant~pernr       = attendant_comm~pernr
                                                               AND attendant_comm~endda >= @sy-datum
                                                               AND attendant_comm~usrty  = '0001' ) ON  zifcu~attendant_pernr =
                                                                                                        attendant~pernr
                                                                                                    AND attendant~endda >= @sy-datum ) ON  eq~equnr =
                                                                                                                                           zifcu~equnr
                                                                                                                                       AND ifcu~posting_date
                                                                                                                                           = zifcu~posting_date
                                                                                                                                       AND ifcu~posting_time
                                                                                                                                           = zifcu~posting_time
                                             LEFT OUTER JOIN
                                                " ---------------------------------------------------------------------
                                                " 70.1: Employee Data (Non-Staff Collection)
                                                " ---------------------------------------------------------------------
                                               zifcu_c_non_staf AS non_staff ON  zifcu~pernr      = non_staff~pernr
                                                                             AND non_staff~status = 'X'

    " ---------------------------------------------------------------------
    " 80: Output fields
    " ---------------------------------------------------------------------
      FIELDS eq~equnr                                                                                                    AS equnr,
             stn~station_t                                                                                               AS station,
             stn_t~type_text                                                                                             AS station_name,
             t_mat~fluid_type                                                                                            AS fuel_type,
             ms~menge                                                                                                    AS quantity,
             ms~meins                                                                                                    AS uom,
             zifcu~pernr                                                                                                 AS collector_staff_id,
             concat_with_space( collector~vorna , collector~nachn , 1 )                                                  AS collector_full_name,
             zifcu~attendant_pernr                                                                                       AS attendant_staff_id,
             attendant_comm~usrid                                                                                        AS attendant_user_id,
             concat_with_space( attendant~vorna, attendant~nachn , 1  )                                                  AS attendant_full_name,
             ifcu~posting_date                                                                                           AS posting_date,
             ifcu~posting_time                                                                                           AS posting_time,
             ifcu~mblnr                                                                                                  AS mblnr,
             ifcu~mjahr                                                                                                  AS mjahr,
             CASE
             WHEN    zifcu~short_text IS NULL
                  OR zifcu~short_text  = @space
                 THEN '@MG@'
                 ELSE '@0P@'
             END                                                                                                         AS remarks,
             CASE
             WHEN    zifcu~doknr IS NULL
                  OR zifcu~doknr  = @space
                 THEN '@MG@'
                 ELSE '@FM@'
             END                                                                                                         AS attachment,
             CASE
             WHEN non_staff~pernr IS NULL
             THEN 'Staff Collection'
             ELSE 'Non-Staff Collection' END                                                                             AS collection_type,
*             CASE mdoc_hr~recdu
*             WHEN 'STD'
*             THEN CAST( unit_conversion( quantity    = CAST( mdoc_hr~readg AS DEC ),
*             source_unit = unit`S`  ,
*             target_unit = unit`H`,
*             client      = @sy-mandt
*             )  AS DEC )
*             ELSE
*             CAST(  mdoc_hr~readg AS DEC )
*             END AS mread_hr_si,

             division( CAST( mdoc_hr~readg AS DEC ) , 3600 , 0 )                                                         AS mread_hr_si,
*             CASE mdoc_hr~recdu
*             WHEN 'STD'
*             THEN CAST( unit_conversion( quantity    = CAST( mdoc_hr~recdv AS DEC ),
*             source_unit = unit`S`  ,
*             target_unit = unit`H ` ,
*             client      = @sy-mandt  )  AS DEC )
*             ELSE
*             CAST(  mdoc_hr~recdv AS DEC )
*             END AS mread_hr_un,
             CAST( mdoc_hr~recdv AS DEC )                                                                                AS mread_hr_un,
             division( CAST( mdoc_hr~cntrr AS DEC ) , 3600 , 0 )                                                         AS cread_hr_si,
*             CASE mdoc_hr~recdu
*             WHEN 'STD'
*             THEN CAST( unit_conversion( quantity    = CAST( mdoc_hr~cdiff AS DEC ),
*             source_unit = unit`S`  ,
*             target_unit = unit`H ` ,
*             client      = @sy-mandt )
*             AS DEC )
*             ELSE
*             CAST(  mdoc_hr~cdiff AS DEC )
*             END  AS cdiff_hr_si,
             division( CAST( mdoc_hr~cdiff AS DEC ) , 3600 , 0 )                                                         AS cdiff_hr_si,
             CASE
             WHEN mdoc_hr~cdiffi = @abap_false AND division( CAST( mdoc_hr~readg AS DEC ) , 3600 , 0 ) > 0 THEN '@5C@' "@5D@
             ELSE '@5B@'
             END                                                                                                         AS diff_hr_flag,
             division( CAST( mdoc_km~readg AS DEC ) , 1000 , 0 )                                                         AS mread_km_si,
             CAST( mdoc_km~recdv AS DEC )                                                                                AS mread_km_un,
             division( CAST( mdoc_km~cntrr AS DEC ) , 1000 , 0 )                                                         AS cread_km_si,
             division( CAST( mdoc_km~cdiff AS DEC ) , 1000 , 0 )                                                         AS cdiff_km_si,
             CASE
             WHEN mdoc_km~cdiffi = @abap_false AND division( CAST( mdoc_km~readg AS DEC ) , 1000 , 0 ) > 0   THEN '@5C@' "@5D@
             ELSE '@5B@'
             END                                                                                                         AS diff_km_flag,
             CAST( mdoc_l~readg AS DEC( 13,3 ) ) * 1000                                                                          AS mread_l_si,
             CAST( mdoc_l~recdv AS DEC )                                                                                 AS mread_l_un,
             CAST( mdoc_l~cntrr AS DEC( 13,3 ) ) * 1000                                                                          AS cread_l_si,
             CAST( mdoc_l~cdiff AS DEC( 13,3 ) ) * 1000                                                                          AS cdiff_l_si,
             CASE
             WHEN mdoc_l~cdiffi = @abap_false AND CAST( mdoc_l~readg AS DEC ) > 0 THEN '@5C@' "@5D@
             ELSE '@5B@'
             END                                                                                                         AS diff_l_flag,
             CAST( mdoc_temp~readg AS DEC )                                                                              AS mread_temp_si,
             CAST( mdoc_temp~recdv AS DEC )                                                                              AS mread_temp_un,
             CAST( mdoc_temp~cntrr AS DEC )                                                                              AS cread_temp_si,
             CAST( mdoc_temp~cdiff AS DEC )                                                                              AS cdiff_temp_si,
             CASE
             WHEN mdoc_temp~cdiffi = @abap_false AND CAST( mdoc_temp~readg AS DEC ) > 0  THEN '@5C@' "@5D@
             ELSE '@5B@'
             END                                                                                                         AS diff_temp_flag,
             CAST( mdoc_l100~readg AS DEC )                                                                              AS mread_l100_si,
             CAST( mdoc_l100~recdv AS DEC )                                                                              AS mread_l100_un,
             CAST( mdoc_l100~cntrr AS DEC )                                                                              AS cread_l100_si,
             CAST( mdoc_l100~cdiff AS DEC )                                                                              AS cdiff_l100_si,
             CASE
             WHEN mdoc_l100~cdiffi = @abap_false AND CAST( mdoc_l100~readg AS DEC ) > 0 THEN '@5C@' "@5D@
             ELSE '@5B@'
             END                                                                                                         AS diff_l100_flag,
             'H'                                                                                                         AS uom_hr,
             'KM'                                                                                                        AS uom_km,
             'L'                                                                                                         AS uom_l,
             'GC'                                                                                                        AS uom_temp,
             'LHK'                                                                                                       AS uom_l100

      WHERE eq~equnr          IN @Me->lr_requnr
        AND stn~station_t     IN @Me->lr_station_t
        AND ifcu~posting_date IN @me->lr_posting_date
        AND ( zifcu~equnr IS NULL OR ( zifcu~equnr IS NOT NULL AND zifcu~pernr           IN @me->lr_collector_id ) )
        AND ( zifcu~equnr IS NULL OR ( zifcu~equnr IS NOT NULL AND zifcu~attendant_pernr IN @me->lr_attendant_id ) )

      INTO CORRESPONDING FIELDS OF TABLE @gt_output.
    DELETE gt_output WHERE collector_staff_id NOT IN me->lr_collector_id.
    DELETE gt_output WHERE attendant_staff_id NOT IN me->lr_attendant_id.
    SORT gt_output BY posting_date DESCENDING
                      posting_time DESCENDING.
    " ---------------------------------------------------------------------
    " 90: Raise exception if no records found
    " ---------------------------------------------------------------------
    IF lines( gt_output ) = 0.
      RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_null_records ).
    ENDIF.
  ENDMETHOD.

  METHOD show_attachment.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------

    " ---------------------------------------------------------------------
    " 20: Check attachment flag
    " ---------------------------------------------------------------------
    CHECK xsdbool( is_output-attachment = '@FM@' ) = abap_true.
    " ---------------------------------------------------------------------
    " 30: Get Attachment info
    " ---------------------------------------------------------------------
    SELECT SINGLE
      FROM zifcu_tech_obj AS zobj
             INNER JOIN
               t370fld_stn AS stn ON zobj~station
                                     = stn~station
                 INNER JOIN
                   draw ON  zobj~dokar = draw~dokar
                        AND zobj~doknr = draw~doknr
                        AND zobj~doktl = draw~doktl
                        AND zobj~dokvr = draw~dokvr

      FIELDS draw~dokar,
             draw~doknr,
             draw~doktl,
             draw~dokvr
      WHERE posting_date  = @is_output-posting_date
        AND posting_time  = @Is_output-posting_time
        AND equnr         = @is_output-equnr
        AND stn~station_t = @Is_output-station
      INTO ( @DATA(lf_doc_type), @DATA(lf_doc_no), @DATA(lf_doc_part), @DATA(lf_doc_version) ).
    " ---------------------------------------------------------------------
    " 40: Show Attachment
    " ---------------------------------------------------------------------
    IF xsdbool( lf_doc_no IS NOT INITIAL AND lf_doc_type IS NOT INITIAL ) = abap_false.
      RETURN.
    ENDIF.
    CALL FUNCTION 'CVAPI_DOC_VIEW'
      EXPORTING  pf_dokar    = lf_doc_type
                 pf_doknr    = lf_doc_no
                 pf_dokvr    = lf_doc_version
                 pf_doktl    = lf_doc_part
      EXCEPTIONS error       = 1
                 not_found   = 2
                 no_auth     = 3
                 no_original = 4
                 OTHERS      = 5.
    " ---------------------------------------------------------------------
    " 50: Raise exception if error occurred
    " ---------------------------------------------------------------------
    CASE sy-subrc.
      WHEN 0.
        RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_doc_success ).
      WHEN 2.
        RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_null_document ).
      WHEN 3.
        RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_auth_fail_doc ).
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_eam_exception( textid = zcx_eam_exception=>eam_other_doc_error ).
    ENDCASE.
  ENDMETHOD.

  METHOD show_grid.
    TRY.
        " ---------------------------------------------------------------------
        " 10: Define structure
        " ---------------------------------------------------------------------
        DATA lo_str      TYPE REF TO cl_abap_structdescr.
        DATA lf_imrg_str TYPE tabname VALUE 'ZIFCU_IMRG_FIELDS'.
        lo_str ?= cl_abap_structdescr=>describe_by_data( VALUE t_output( )  ).
        go_columns = go_alv->get_columns( ).
        " ---------------------------------------------------------------------
        " 20.1: Get field customizing
        " ---------------------------------------------------------------------
        DATA(lf_tcode) = cl_abap_syst=>get_transaction_code( ).
        SELECT FROM zifcu_c_rpt_fld
          FIELDS *
          WHERE aktiv
                = @abap_true
            AND report  = @sy-cprog
            AND tcode   = @lf_tcode
            AND tabname = @lf_imrg_str
          INTO TABLE @DATA(lt_field_cust).
        " ---------------------------------------------------------------------
        " 20.2: Set field catalog
        " ---------------------------------------------------------------------
        DO lines( lo_str->components ) TIMES.
          ASSIGN lo_str->components[ sy-index ] TO FIELD-SYMBOL(<fs_component>).
          CASE <fs_component>-name.
            WHEN 'MBLNR' OR 'MJAHR'.
              _thiscolumn.
              go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
            WHEN 'COLLECTOR_STAFF_ID'.
              _thiscolumn.
              go_column->set_long_text( 'Collector' ).
              go_column->set_short_text( 'Collector' ).
              go_column->set_medium_text( 'Collector' ).
            WHEN 'COLLECTION_TYPE'.
              _thiscolumn.
              go_column->set_long_text( 'Collection Type' ).
              go_column->set_short_text( 'Collection' ).
              go_column->set_medium_text( 'Collection Type' ).
              go_column->set_output_length( 20 ).
            WHEN 'COLLECTOR_FULL_NAME'.
              _thiscolumn.
              go_column->set_long_text( |Collector's Full Name | ).
              go_column->set_short_text( |Collector's Full Name | ).
              go_column->set_medium_text( |Collector's Full Name | ).
              go_column->set_output_length( 30 ).
            WHEN 'ATTENDANT_STAFF_ID'.
              _thiscolumn.
              go_column->set_long_text( 'Attendant' ).
              go_column->set_short_text( 'Attendant' ).
              go_column->set_medium_text( 'Attendant' ).
            WHEN 'ATTENDANT_FULL_NAME'.
              _thiscolumn.
              go_column->set_long_text( |Attendant's Full Name | ).
              go_column->set_short_text( |Attendant's Full Name | ).
              go_column->set_medium_text( |Attendant's Full Name | ).
              go_column->set_output_length( 30 ).
            WHEN 'ATTENDANT_USER_ID'.
              _thiscolumn.
              go_column->set_long_text( |Att. User ID | ).
              go_column->set_short_text( |Att. User ID | ).
              go_column->set_medium_text( |Att. User ID | ).
            WHEN 'REMARKS'.
              _thiscolumn.
              go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
              go_column->set_long_text( |Remarks | ).
              go_column->set_short_text( |Remarks | ).
              go_column->set_medium_text( |Remarks | ).
            WHEN 'ATTACHMENT'.
              _thiscolumn.
              go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
              go_column->set_long_text( |Attachment | ).
              go_column->set_short_text( |Attachment | ).
              go_column->set_medium_text( |Attachment | ).
            WHEN 'FUEL_TYPE' OR 'EQUNR' OR 'STATION' OR 'QUANTITY' OR 'STATION_NAME'.
              _thiscolumn.
              go_column->set_optimized( abap_true ).
            WHEN OTHERS.
              _thiscolumn.
              IF line_exists( lt_field_cust[ fieldname = <fs_component>-name  ] ).
                ASSIGN lt_field_cust[ fieldname = <fs_component>-name ] TO FIELD-SYMBOL(<fs_fname>).
                go_column->set_long_text( <fs_fname>-scrtext_l ).
                go_column->set_short_text( <fs_fname>-scrtext_s ).
                go_column->set_medium_text( <fs_fname>-scrtext_m ).
                IF <fs_fname>-hidden = abap_true.
                  go_column->set_visible( abap_false ).
                ENDIF.

              ENDIF.
          ENDCASE.
        ENDDO.
        " ---------------------------------------------------------------------
        " 30: Selection
        " ---------------------------------------------------------------------
        go_select = go_alv->get_selections( ).
        IF xsdbool(  go_select IS NOT INITIAL ) = abap_true.
          go_select->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        ENDIF.
*        go_alv->get_columns( )->set_optimize( abap_true ).
        go_alv->display( ).

      " ---------------------------------------------------------------------
      " 40: Exception
      " ---------------------------------------------------------------------
      CATCH cx_salv_not_found INTO DATA(lo_alv_ex).
        RAISE EXCEPTION NEW zcx_eam_exception( textid   = zcx_eam_exception=>eam_alv_error
                                               previous = lo_alv_ex ).
    ENDTRY.
  ENDMETHOD.

  METHOD show_remarks.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_text TYPE TABLE OF char255.

    " ---------------------------------------------------------------------
    " 20: Check remarks
    " ---------------------------------------------------------------------
    CHECK xsdbool( is_output-remarks = '@0P@' ) = abap_true.
    " ---------------------------------------------------------------------
    " 30: Get text
    " ---------------------------------------------------------------------
    SELECT SINGLE
      FROM zifcu_tech_obj AS zobj
             INNER JOIN
               t370fld_stn AS stn ON zobj~station
                                     = stn~station
      FIELDS short_text
      WHERE posting_date  = @is_output-posting_date
        AND posting_time  = @Is_output-posting_time
        AND equnr         = @is_output-equnr
        AND stn~station_t = @Is_output-station
      INTO @DATA(lf_short_text).

    IF xsdbool( lf_short_text IS NOT INITIAL AND lf_short_text <> space )
       = abap_false.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 30: Create text table
    " ---------------------------------------------------------------------
    APPEND INITIAL LINE TO lt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
    <fs_text> = lf_short_text.
    " ---------------------------------------------------------------------
    " 40: Display text
    " ---------------------------------------------------------------------
    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING  endpos_col   = 70
                 endpos_row   = 10
                 startpos_col = 10
                 startpos_row = 5
                 titletext    = CONV char80( |Remarks/Observations| )
      TABLES     valuetab     = lt_text
      EXCEPTIONS break_off    = 1.
    IF sy-subrc <> 1.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
