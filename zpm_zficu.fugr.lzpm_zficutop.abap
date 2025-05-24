FUNCTION-POOL zpm_zficu.                    " MESSAGE-ID ..
" ---------------------------------------------------------------------
" 10: Data Declaration
" ---------------------------------------------------------------------
DATA go_eam_ifcu TYPE REF TO zcl_eam_ifcu.
DATA go_access   TYPE REF TO cl_gui_custom_container.
DATA go_alv      TYPE REF TO cl_salv_table.
DATA go_columns  TYPE REF TO cl_salv_columns_table.
DATA go_column   TYPE REF TO cl_salv_column_table.
DATA go_layout   TYPE REF TO cl_salv_layout.
DATA go_select   TYPE REF TO cl_salv_selections.
DATA go_editor   TYPE REF TO cl_gui_custom_container.
DATA go_t_editor TYPE REF TO cl_gui_textedit.
FIELD-SYMBOLS <fs_any_msg> TYPE bapiret2.
" ---------------------------------------------------------------------
" 20: Screen fields
" ---------------------------------------------------------------------
DATA gf_staff_id      TYPE persno.
DATA gf_staff_name    TYPE pad_name.
DATA gf_show_t_access TYPE flag VALUE 'X'.
DATA gf_valid_from    TYPE datum.
DATA gf_valid_to      TYPE datum.
DATA gf_access_reason TYPE zifcu_acc_reason.
DATA gf_ucomm         TYPE sy-ucomm.
" ---------------------------------------------------------------------
" 30: App objects
" ---------------------------------------------------------------------
DATA gt_staff_access  TYPE zcl_eam_ifcu=>tt_station_access.
DATA gt_stations      TYPE zcl_eam_ifcu=>tt_stations_list.
DATA gt_locations     TYPE zcl_eam_ifcu=>tt_locations_list.

CLASS lcl_pm_ifcu DEFINITION DEFERRED.
DATA go_ifcu TYPE REF TO lcl_pm_ifcu.
" ---------------------------------------------------------------------
" 50: M4cros
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
DEFINE convert_message_to_success.
  LOOP AT &3 ASSIGNING <fs_any_msg>.
    IF xsdbool( <fs_any_msg>-id = &1 AND (
                <fs_any_msg>-number = &2 ) )
       = abap_true.
      <fs_any_msg>-type = 'S'.
    ENDIF.
  ENDLOOP.
END-OF-DEFINITION.
INCLUDE lzpm_zficud01. " Local class definition
