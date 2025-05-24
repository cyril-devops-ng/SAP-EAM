"!
"!<h1>Class: LCL_PM_IFCU</h1>
"!<h2> Local class for persisting global class ZCL_EAM_IFCU</h2>
"!<h3>Methods</h3>
"! <ul>
"! <li>Constructor</li>
"! <li>map_ucomm_to_ifcu_process</li>
"! <li>create_grid</li>
"! <li>show_grid</li>
"! <li>get_staff_access</li>
"! <li>on_added_function</li>
"! <li>on_link_click</li>
"! <li>refresh_grid</li>
"! <li>revoke_access</li>
"! <li>create_access</li>
"! <li>create_temporary_access</li>
"! </ul>
CLASS lcl_pm_ifcu DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING ir_staff_id    TYPE zcl_eam_ifcu=>tr_staff_id OPTIONAL
                                  ir_station     TYPE zcl_eam_ifcu=>tr_station  OPTIONAL
                                  if_temp_access TYPE flag                      OPTIONAL
                                  if_ucomm       TYPE sy-ucomm

                        RAISING   zcx_eam_exception.

    METHODS map_ucomm_to_ifcu_process
      IMPORTING if_ucomm               TYPE sy-ucomm OPTIONAL
      RETURNING VALUE(rf_ifcu_process) TYPE zcl_eam_ifcu=>ifcu_process
      RAISING   zcx_eam_exception.

    METHODS create_grid CHANGING ct_tab TYPE ANY TABLE
                        RAISING  zcx_eam_exception.

    METHODS show_grid
      RAISING zcx_eam_exception.

    METHODS get_staff_access
      RAISING zcx_eam_exception.

    METHODS on_added_function FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

    METHODS on_link_click FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
      IMPORTING !row !column.

    METHODS refresh_grid
      RAISING zcx_eam_exception.

    METHODS revoke_access IMPORTING it_selection TYPE zcl_eam_ifcu=>tt_station_access
                          RAISING   zcx_eam_exception.

    METHODS create_access IMPORTING if_staff_id TYPE persno  OPTIONAL
                                    if_station  TYPE station OPTIONAL
                          RAISING   zcx_eam_exception.

    METHODS create_temporary_access IMPORTING if_staff_id      TYPE persno  OPTIONAL
                                              if_station       TYPE station OPTIONAL
                                              if_valid_from    TYPE begda   OPTIONAL
                                              if_valid_to      TYPE endda   OPTIONAL
                                              if_access_reason TYPE text255 OPTIONAL
                                    RAISING   zcx_eam_exception.

    DATA gr_staff_id    TYPE zcl_eam_ifcu=>tr_staff_id.
    DATA gr_station     TYPE zcl_eam_ifcu=>tr_station.
    DATA gf_temp_access TYPE flag.
    DATA gf_ucomm       TYPE sy-ucomm.

    CONSTANTS gc_create_assignment  TYPE sy-ucomm VALUE 'CREATE_A'.
    CONSTANTS gc_display_assignment TYPE sy-ucomm VALUE 'DISP_A'.
    CONSTANTS gc_revoke_assignment  TYPE sy-ucomm VALUE 'REVOKE_ACCESS'.
    CONSTANTS gc_select_stations    type sy-ucomm value 'SEL_STAT'.
    CONSTANTS gc_create_temp_ass    type sy-ucomm value 'CREATE_TA'.
ENDCLASS.
