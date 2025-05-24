*&---------------------------------------------------------------------*
*& Include          ZEAM_IFCU_STAFF_REPORT_TOP
*&---------------------------------------------------------------------*
" ---------------------------------------------------------------------
" 10: Type Declaration
" ---------------------------------------------------------------------
TYPES: BEGIN OF t_output,
         equnr               TYPE equnr,
         station             TYPE station_t,
         station_name        TYPE stn_type_text,
         fuel_type           TYPE fluid_type,
         quantity            TYPE menge_d,
         uom                 TYPE meins,
         mblnr               TYPE mblnr,
         mjahr               TYPE mjahr,
         collector_staff_id  TYPE persno,
         collector_full_name TYPE pad_cname,
         attendant_staff_id  TYPE persno,
         attendant_user_id   TYPE usnam,
         attendant_full_name TYPE pad_cname,
         collection_type     TYPE char20,
         posting_date        TYPE imrc_idate,
         posting_time        TYPE imrc_itime,
         remarks             TYPE char4,
         attachment          TYPE char4.
         include type ZIFCU_IMRG_FIELDS.
       types: END OF t_output.
TYPES t_st_output     TYPE STANDARD TABLE OF t_output.
TYPES t_sorted_output TYPE SORTED TABLE OF t_output
                      WITH NON-UNIQUE KEY equnr station posting_date posting_time.
" ---------------------------------------------------------------------
" 20: Data Declaration
" ---------------------------------------------------------------------
DATA go_alv     TYPE REF TO cl_salv_table.
DATA go_columns TYPE REF TO cl_salv_columns_table.
DATA go_column  TYPE REF TO cl_salv_column_table.
DATA go_layout  TYPE REF TO cl_salv_layout.
DATA go_cont    TYPE REF TO cl_gui_container.
DATA go_select  TYPE REF TO cl_salv_selections.
DATA gt_output  TYPE t_st_output.
DATA gs_output  TYPE t_output.
FIELD-SYMBOLS <fs_any_msg> TYPE bapiret2.

" ---------------------------------------------------------------------
" 30: Class Definition
" ---------------------------------------------------------------------
CLASS lcl_ifcu_staff_report DEFINITION DEFERRED.

CLASS lcl_ifcu_staff_report DEFINITION.
  PUBLIC SECTION.
    TYPES ty_equnr          TYPE RANGE OF equnr.
    TYPES ty_station_t      TYPE RANGE OF station_t.
    TYPES ty_posting_date   TYPE RANGE OF imrc_idate.
    TYPES ty_collector_id   TYPE RANGE OF persno.
    TYPES ty_attendant_id   TYPE RANGE OF persno.
    TYPES ty_requnr         TYPE RANGE OF equnr.
    TYPES ty_station_t_1    TYPE RANGE OF station_t.
    TYPES ty_posting_date_1 TYPE RANGE OF imrc_idate.
    TYPES ty_collector_id_1 TYPE RANGE OF persno.
    TYPES ty_attendant_id_1 TYPE RANGE OF persno.

    CLASS-METHODS create_report
      IMPORTING if_equnr        TYPE equnr           OPTIONAL
                if_station      TYPE station_t       OPTIONAL
                if_posting_date TYPE imrc_idate      OPTIONAL
                ir_equnr        TYPE ty_requnr       OPTIONAL
                ir_station_t    TYPE ty_station_t    OPTIONAL
                ir_posting_date TYPE ty_posting_date OPTIONAL
                ir_collector_id TYPE ty_collector_id OPTIONAL
                ir_attendant_id TYPE ty_attendant_id OPTIONAL
      RETURNING VALUE(r_result) TYPE REF TO lcl_ifcu_staff_report
      RAISING   zcx_eam_exception.

    CONSTANTS gc_screen TYPE string VALUE '0100'.

    METHODS constructor IMPORTING if_equnr        TYPE equnr      OPTIONAL
                                  if_station      TYPE station_t  OPTIONAL
                                  if_posting_date TYPE imrc_idate OPTIONAL
                        RAISING   zcx_eam_exception.

    METHODS create_grid CHANGING ct_tab TYPE ANY  TABLE
                        RAISING  zcx_eam_exception.

    METHODS show_grid    RAISING zcx_eam_exception.
    METHODS retrieve_set RAISING zcx_eam_exception.

    METHODS on_added_function
      FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

    METHODS on_link_click
      FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
      IMPORTING !row !column.

    METHODS refresh_grid.

    METHODS show_remarks IMPORTING is_output TYPE t_output
                         RAISING   zcx_eam_exception.

    METHODS show_attachment IMPORTING is_output TYPE t_output
                            RAISING   zcx_eam_exception.

  PRIVATE SECTION.
    DATA lr_requnr       TYPE RANGE OF equnr.
    DATA lr_station_t    TYPE RANGE OF station_t.
    DATA lr_posting_date TYPE RANGE OF imrc_idate.
    DATA lr_collector_id TYPE RANGE OF persno.
    DATA lr_attendant_id TYPE RANGE OF persno.

    CONSTANTS gc_ifcu           TYPE sy-tcode VALUE 'ZIFCU'.
    CONSTANTS gc_refresh        TYPE ui_func  VALUE 'REFRESH'.
    CONSTANTS gc_mult_rec_error TYPE string   VALUE '(Multiple Records) - Requires filter criteria.'.
    CONSTANTS gc_sing_rec_error TYPE string   VALUE '(Single Record Read) - Requires filter criteria.'.
ENDCLASS.
" ---------------------------------------------------------------------
" 50: Macros
" ---------------------------------------------------------------------
DEFINE _thiscolumn.
  go_column ?= go_columns->get_column( <fs_component>-name ).
END-OF-DEFINITION.
DEFINE add_func.
  go_alv->get_functions( )->add_function( tooltip  = &1
                                          name     = &2
                                          icon     = &3
                                          text     = &4
                                          position = 1 ).
END-OF-DEFINITION.
DEFINE deactivate_screen.
  screen-input     = '0'.
  screen-active    = '0'.
  screen-invisible = '1'.
END-OF-DEFINITION.
DEFINE activate_screen.
  screen-input     = '1'.
  screen-active    = '1'.
  screen-invisible = '0'.
END-OF-DEFINITION.
DEFINE configure_single_screen.
  LOOP AT SCREEN.
    IF xsdbool( screen-group1 = `DC` )
       = abap_true.
      deactivate_screen.
    ENDIF.
    IF xsdbool( screen-group1 = 'SC' )
       = abap_true.
      activate_screen.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
END-OF-DEFINITION.
DEFINE configure_multiple_screen.
  LOOP AT SCREEN.
    IF xsdbool( screen-group1 = `SC` )
       = abap_true.
      deactivate_screen.
    ENDIF.
    IF xsdbool( screen-group1 = 'DC' )
       = abap_true.
      activate_screen.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
END-OF-DEFINITION.
