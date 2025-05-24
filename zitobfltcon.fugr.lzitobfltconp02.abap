*&---------------------------------------------------------------------*
*&  Include           LITOBFLTCONP02
*&---------------------------------------------------------------------*
CLASS lcl_documents_output IMPLEMENTATION.
************************************************************************
* Method Class Constructor
*-----------------------------------------------------------------------
* Purpose: Constructor for local class to display selected IFCU measurements
************************************************************************
  METHOD class_constructor.
* call alv to display documents related to the technical object
*--- singelton object -> only one instance
    CREATE OBJECT go_lcl_documents_output.

  ENDMETHOD.   " class_constructor.
************************************************************************
* Method get_reference
*-----------------------------------------------------------------------
* Purpose: get reference of this class
************************************************************************
  METHOD get_reference.
* get reference of this local class
    ro_lcl_documents_output = go_lcl_documents_output.

  ENDMETHOD.   " get_reference.
************************************************************************
* Method call_screen
*-----------------------------------------------------------------------
* Purpose: call the screen to display the documents of a technical object
************************************************************************
  METHOD call_screen.
* fill required global data for later processing
    gt_docs  = it_docs. " insert selected IFCU documents
    gv_objnr = iv_objnr.
    gv_datee = iv_datee. " selection end date
    gv_dates = iv_dates. " selection start date

* call alv to display the IFCU measurements related to the technical object
    CALL SCREEN 0205.

  ENDMETHOD.   " call_screen.
************************************************************************
* Method tree_pbo
*-----------------------------------------------------------------------
* Purpose: called in PBO of the ALV Tree dynpro, initialize and flush
*          to display the IFCU measurements
************************************************************************
  METHOD tree_pbo.
* initialize tree
    go_lcl_documents_output->init_tree( ).

    cl_gui_cfw=>flush( ).

  ENDMETHOD.   " tree_pbo.
************************************************************************
* Method init_tree
*-----------------------------------------------------------------------
* Purpose: called in PBO of the ALV Tree dynpro, prepare display of tree
*          with IFCU measurements
************************************************************************
  METHOD init_tree.
* data declaration
    DATA:
          ls_hierarchy_header  TYPE treev_hhdr,
          lt_list_commentary   TYPE slis_t_listheader,
          lt_fieldcat          TYPE lvc_t_fcat,
          lt_toolbar_excluding TYPE ui_functions,
          lt_events            TYPE cntl_simple_events,
          ls_event             TYPE cntl_simple_event.

* if tree is already initialized -> nothing to do.
    IF go_tree IS NOT INITIAL.
      RETURN.
    ENDIF.

    BREAK-POINT ID eam_tzs_gen.
    IF gv_time_zone_active NE tzs1_active-yes.
*--- time zone support not active: deactivate function
      APPEND tzs1_fcode_button TO gt_fcode_excl.
    ENDIF.

* if tree is not initialized, do this now
    SET TITLEBAR 'TREE' WITH text-ifa.
    SET PF-STATUS 'TREE' EXCLUDING gt_fcode_excl.

* create tree control
    CREATE OBJECT go_tree
      EXPORTING
        parent                      = cl_gui_container=>screen0
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
        item_selection              = abap_true
        no_html_header              = abap_false
        no_toolbar                  = abap_false
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.
    IF sy-subrc IS NOT INITIAL.
* => bug, should not happen
      MESSAGE a899(ig) WITH sy-repid 'INIT_TREE' 'CREATE OBJECT' sy-subrc.
    ENDIF.

* create field catalog
    me->create_fieldcat( IMPORTING et_fieldcat = lt_fieldcat ).

* create hierarchy header
    me->create_hierarchy_header( IMPORTING es_hierarchy_header = ls_hierarchy_header ).

* create info area
    me->create_list_commentary( IMPORTING et_list_commentary = lt_list_commentary ).

* exclude sum function from toolbar
    APPEND cl_gui_alv_tree=>mc_fc_calculate        TO lt_toolbar_excluding.
    APPEND cl_gui_alv_tree=>mc_fc_maintain_variant TO lt_toolbar_excluding.
    APPEND cl_gui_alv_tree=>mc_fc_current_variant  TO lt_toolbar_excluding.
    APPEND cl_gui_alv_tree=>mc_fc_load_variant     TO lt_toolbar_excluding.

* create empty tree
    go_tree->set_table_for_first_display(
      EXPORTING
        is_hierarchy_header  = ls_hierarchy_header
        it_list_commentary   = lt_list_commentary
        it_toolbar_excluding = lt_toolbar_excluding
      CHANGING
        it_outtab            = gt_tree_tab "table must be empty !!
        it_fieldcatalog      = lt_fieldcat ).

*--- set event handler
    go_tree->get_registered_events(
               IMPORTING events = lt_events ).

    ls_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
    APPEND ls_event TO lt_events.

    go_tree->set_registered_events(
               EXPORTING events = lt_events ).

* set handler for double click, to go to transaction of measurement/goods movement documents
    SET HANDLER me->handle_double_click_tree FOR go_tree.

* create icons for nodes
    me->create_icons( ).

* create hierarchy
    me->create_hierarchy( ).

* update frontend
    go_tree->frontend_update( ).

* adjust column_width (only optimize columns, not tree)
    go_tree->column_optimize( ).

  ENDMETHOD.   " init_tree .
************************************************************************
* Method create_fieldcat
*-----------------------------------------------------------------------
* Purpose: Create fieldcatalogue for tree to display IFCU measurements
************************************************************************
  METHOD create_fieldcat.
* data declaration
    DATA:
          ls_fieldcat TYPE lvc_s_fcat.

* Create fieldcatalogue

* Document Number
    ls_fieldcat-fieldname = 'NUMBER'.
    ls_fieldcat-tabname   = 'GT_TREE_TAB'.
    ls_fieldcat-no_zero   = abap_true.
    ls_fieldcat-seltext   = text-doc.
    ls_fieldcat-coltext   = text-doc.
    ls_fieldcat-outputlen = 20.
    APPEND ls_fieldcat TO et_fieldcat.

    CLEAR ls_fieldcat.

* Document Year
    ls_fieldcat-fieldname = 'DOC_YEAR'.
    ls_fieldcat-tabname   = 'GT_TREE_TAB'.
    ls_fieldcat-ref_field = 'MJAHR'.
    ls_fieldcat-ref_table = 'MKPF'.
    APPEND ls_fieldcat TO et_fieldcat.

    CLEAR ls_fieldcat.

* Position
    ls_fieldcat-fieldname = 'POSITION'.
    ls_fieldcat-tabname   = 'GT_TREE_TAB'.
    ls_fieldcat-ref_field = 'PSORT'.
    ls_fieldcat-ref_table = 'IMPTT'.
    APPEND ls_fieldcat TO et_fieldcat.

    CLEAR ls_fieldcat.

* Position text
    ls_fieldcat-fieldname = 'POS_TXT'.
    ls_fieldcat-tabname   = 'GT_TREE_TAB'.
    ls_fieldcat-ref_field = 'PTTXT'.
    ls_fieldcat-ref_table = 'IMPTT'.
    APPEND ls_fieldcat TO et_fieldcat.

    CLEAR ls_fieldcat.

* Value
    ls_fieldcat-fieldname = 'VALUE'.
    ls_fieldcat-tabname   = 'GT_TREE_TAB'.
    ls_fieldcat-seltext   = text-val.
    ls_fieldcat-coltext   = text-val.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO et_fieldcat.

    CLEAR ls_fieldcat.

* measurement unit
    ls_fieldcat-fieldname = 'MEAS_UNIT'.
    ls_fieldcat-tabname   = 'GT_TREE_TAB'.
    ls_fieldcat-ref_field = 'RECDU'.
    ls_fieldcat-ref_table = 'IMRG'.
    APPEND ls_fieldcat TO et_fieldcat.

    CLEAR ls_fieldcat.

* Flag - Marked for reverse
    ls_fieldcat-fieldname = 'REV_FLAG'.
    ls_fieldcat-tabname   = 'GT_TREE_TAB'.
    ls_fieldcat-seltext   = text-rfl.
    ls_fieldcat-coltext   = text-rfl.
    ls_fieldcat-outputlen = 2.
    ls_fieldcat-icon      = abap_true.
    APPEND ls_fieldcat TO et_fieldcat.

    CLEAR ls_fieldcat.

  ENDMETHOD.   " create_fieldcat.
************************************************************************
* Method create_hierarchy
*-----------------------------------------------------------------------
* Purpose: create hierarchy for Tree display of IFCU measurements
************************************************************************
  METHOD create_hierarchy.
* data declaration
    DATA:
          lt_output             TYPE lcl_documents_output=>gty_t_ifcu_doc_data,
          lv_date_old           TYPE imrc_idate,
          lv_time_old           TYPE imrc_itime,
          lv_parent_node        TYPE lvc_nkey,
          lv_node               TYPE lvc_nkey,
          ls_tree_output        LIKE LINE OF gt_tree_output,
          ls_output             LIKE LINE OF lt_output.

    FIELD-SYMBOLS: <ls_output>  LIKE LINE OF lt_output.

    lt_output = gt_docs.
* main part ------------------------------------------------------------

* For all IFCU measurements documents and goods movement documents do the following:
    LOOP AT lt_output ASSIGNING <ls_output>.
      CLEAR ls_tree_output.

* create root node if time/date combination is new (new root node)
      IF lv_date_old NE <ls_output>-meas_date OR
         lv_time_old NE <ls_output>-meas_time.
        CLEAR ls_output. " make sure not old data is in output variable
* set variables to create node with correct data
        ls_output-meas_date = <ls_output>-meas_date.
        ls_output-meas_time = <ls_output>-meas_time.
* create root node
        me->create_node(
          EXPORTING iv_parent_node = ''   " no parent node for a root node
          IMPORTING ev_node        = lv_node
          CHANGING  cs_output      = ls_output ).
        lv_parent_node = lv_node.

* save root node ID and corresponding output ID
        ls_tree_output-node = lv_node.
        ls_tree_output-data-meas_date = <ls_output>-meas_date.
        ls_tree_output-data-meas_time = <ls_output>-meas_time.
        INSERT ls_tree_output INTO TABLE gt_tree_output.
      ENDIF.

* do the following for all IFCU measurements under the root node created for this
* time/date combination

* Set output data for sub node and the created root node as parent node
      ls_tree_output-parent_node = lv_parent_node.
      ls_output-meas_date = <ls_output>-meas_date.
      ls_output-meas_time = <ls_output>-meas_time.
      me->create_node(
        EXPORTING iv_parent_node = lv_parent_node
        IMPORTING ev_node        = lv_node
        CHANGING  cs_output      = <ls_output> ).

* save node ID and corresponding output ID of the sub node
      ls_tree_output-node = lv_node.
      ls_tree_output-data = <ls_output>.
      INSERT ls_tree_output INTO TABLE gt_tree_output.

* expand small trees (only parent nodes with subnodes)
      IF LINES( lt_output ) LE gc_expand_tree_lines AND
         ls_tree_output-parent_node IS NOT INITIAL.
        go_tree->expand_node(
                   EXPORTING i_node_key = lv_parent_node ).
      ENDIF.

* store current date/time combination in local variables, that next loop step
* knows, if a new root node needs to be created, or the next IFCU measurement
* belongs to the same root node
      lv_date_old = <ls_output>-meas_date.
      lv_time_old = <ls_output>-meas_time.
    ENDLOOP.
  ENDMETHOD.   " create_hierarchy.
************************************************************************
* Method create_node
*-----------------------------------------------------------------------
* Purpose: Create a node for hierarchy create
************************************************************************
  METHOD create_node.
* data declaration
    DATA: lv_node_text      TYPE lvc_value,
          ls_node_layout    TYPE lvc_s_layn,
          lv_icon           TYPE tv_image,
          lv_time_temp(8)   TYPE c,
          lv_date_temp(11)  TYPE c.

    CONSTANTS:
           lc_doc_type_m    TYPE c LENGTH 1 VALUE 'M', " measurement documents
           lc_doc_type_g    TYPE c LENGTH 1 VALUE 'G'. " Goods movement

* main part ------------------------------------------------------------
* handling for root nodes / sub nodes
    IF iv_parent_node IS INITIAL.
* handling for root node -> display technical object (use convertion exit)
      WRITE cs_output-meas_date TO lv_date_temp.
      WRITE cs_output-meas_time TO lv_time_temp.
      CONCATENATE lv_date_temp lv_time_temp INTO lv_node_text SEPARATED BY space.
      lv_icon                 = gs_icon-date.
      ls_node_layout-disabled = abap_false.
    ELSE.
* handling sub node -> display IFCU documents of a time/date combination
* measurement documents
      IF cs_output-doc_type EQ lc_doc_type_m.
        lv_icon      = gs_icon-mdoc.
        lv_node_text = gv_label_mdoc.
      ENDIF.
* goods movement documents
      IF cs_output-doc_type EQ lc_doc_type_g.
        lv_icon      = gs_icon-gods.
        lv_node_text = text-bgm.
      ENDIF.
      ls_node_layout-disabled = abap_false.
    ENDIF.

* Set other node properties
    ls_node_layout-n_image   = lv_icon.
    ls_node_layout-exp_image = lv_icon.
* create node
    go_tree->add_node(
      EXPORTING
        i_relat_node_key = iv_parent_node
        i_relationship   = iv_relationship
        i_node_text      = lv_node_text
        is_outtab_line   = cs_output
        is_node_layout   = ls_node_layout
      IMPORTING
        e_new_node_key   = ev_node ).

  ENDMETHOD.   " create_node.
************************************************************************
* Module  CREATE_ICONS
*-----------------------------------------------------------------------
* Purpose: create icons with tooltips for output
************************************************************************
  METHOD create_icons.
* data declaration
    DATA: lv_label TYPE string.

    CONSTANTS:
          lc_meas_doc TYPE dfies-fieldname VALUE 'IMRC_MDOCM'.

* main part ------------------------------------------------------------

* Create icons for IFCU documents (measurement and goods movement documents)
* get label
    CALL FUNCTION 'DDIF_FIELDLABEL_GET'
      EXPORTING
        tabname = lc_meas_doc
        langu   = sy-langu
      IMPORTING
        label   = lv_label
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc NE 0.
      CLEAR lv_label.
    ELSE.
      gv_label_mdoc = lv_label.
    ENDIF.

* create icon for measurement documents
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = icon_ppe_aenode
        text                  = space
        info                  = lv_label
        add_stdinf            = 'X'
      IMPORTING
        RESULT                = gs_icon-mdoc
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc = 2.
      gs_icon-mdoc = icon_ppe_aenode.
    ENDIF.

* create icon for goods movement
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = icon_delivery
        text                  = space
        info                  = text-bgm
        add_stdinf            = 'X'
      IMPORTING
        RESULT                = gs_icon-gods
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc = 2.
      gs_icon-gods = icon_delivery.
    ENDIF.

* create icon for date
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = icon_date
        text                  = space
        info                  = text-hht
      IMPORTING
        RESULT                = gs_icon-date
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc = 2.
      gs_icon-date = icon_date.
    ENDIF.

* create icon for cancel
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = icon_storno
        text                  = space
*        info                  = text-hht
        add_stdinf            = 'X'
      IMPORTING
        RESULT                = gs_icon-storno
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc = 2.
      gs_icon-storno = icon_storno.
    ENDIF.

  ENDMETHOD.                    "create_icons
************************************************************************
* Module  CREATE_HIERARCHY_HEADER
*-----------------------------------------------------------------------
* Purpose: Header Area above tree
************************************************************************
  METHOD create_hierarchy_header.
* main part - set properties for header area above the tree with the IFCU
* measurements
    es_hierarchy_header-heading   = text-hht.
    es_hierarchy_header-tooltip   = text-hht.
    es_hierarchy_header-width     = 30.
    es_hierarchy_header-width_pix = ''.

  ENDMETHOD.                    "create_hierarchy_header
************************************************************************
* Module  CREATE_LIST_COMMENTARY
*-----------------------------------------------------------------------
* Purpose: create commentary (listheader)
************************************************************************
  METHOD create_list_commentary.
    DATA:  ls_line                     TYPE slis_listheader,
           lv_equnr                    TYPE ionra-equnr.

    CONSTANTS:
           lc_header_type_selection(1) TYPE c VALUE 'S'.

* set header texts for alv tree, to show additional information
    ls_line-typ = lc_header_type_selection.

* Display technical object
* Display Equipment
    IF gv_objnr(2) EQ 'IE'.
* transform objnr to Equi number-> in error case display nothing
      CALL FUNCTION 'OBJECT_KEY_GET_IE'
        EXPORTING
          objnr  = gv_objnr
        IMPORTING
          equnr  = lv_equnr
        EXCEPTIONS
          OTHERS = 0.

* Display Equi Number in first row
      WRITE text-equ   TO ls_line-key.
      WRITE gv_objnr+2 TO ls_line-info.
      APPEND ls_line   TO et_list_commentary.

* read shorttext of equi
      IF gv_objtxt IS INITIAL.
        CALL FUNCTION 'IREP1_EQUIPMENT_TEXT_READ'
          EXPORTING
            i_equnr = lv_equnr
          IMPORTING
            e_eqktx = gv_objtxt
          EXCEPTIONS
            OTHERS  = 0.
      ENDIF.

* Display Equi shorttext in second row
      WRITE text-txt  TO ls_line-key.
      WRITE gv_objtxt TO ls_line-info.
      APPEND ls_line  TO et_list_commentary.
    ENDIF.

* display selection start time
    MOVE text-da1 TO ls_line-key.
    WRITE gv_dates TO ls_line-info.
    APPEND ls_line TO et_list_commentary.

* display selection end time
    MOVE text-da2 TO ls_line-key.
    WRITE gv_datee TO ls_line-info.
    APPEND ls_line TO et_list_commentary.

  ENDMETHOD.                    "create_list_commentary
************************************************************************
* Module  TREE_PAI
*-----------------------------------------------------------------------
* Purpose: PAI Handling for ALV Tree
************************************************************************
  METHOD tree_pai.
* main part -> execute user command in tree (exit, mark for cancel, ...)
    go_lcl_documents_output->execute_user_command_tree( EXPORTING iv_ucomm = iv_ucomm ).

  ENDMETHOD.                    "tree_pai
************************************************************************
* Module  EXECUTE_USER_COMMAND
*-----------------------------------------------------------------------
* Purpose: User Command Handling
************************************************************************
  METHOD execute_user_command_tree.
* data declaration
    DATA: lv_answer(1)  TYPE c.

* constants
    CONSTANTS: lc_answer_yes(1)    TYPE c VALUE 'J',
               lc_answer_no(1)     TYPE c VALUE 'N',
               lc_answer_cancel(1) TYPE c VALUE 'A'.

* sy-ucomm handling
    CASE iv_ucomm.

* BACK
      WHEN gc_ucomm_back.
* if there had been changes, show popup
        IF gv_save_needed = abap_true.
          CALL FUNCTION 'POPUP_TO_CONFIRM_DATA_LOSS'
            EXPORTING
              titel  = text-pop
            IMPORTING
              answer = lv_answer.
* save changes if user pressed on save yes
          IF lv_answer = lc_answer_yes.
            me->save_changes( ).
            me->end_processing( ).
            LEAVE TO SCREEN 0.
*!!!            lcl_selection_screen=>call_selection_screen( iv_objnr = gv_objnr ).
* do not save if user pressed on no save
          ELSEIF lv_answer EQ lc_answer_no.
            me->end_processing( ).
            LEAVE TO SCREEN 0.
*!!!            lcl_selection_screen=>call_selection_screen( iv_objnr = gv_objnr ).
* do nothing, if user press on abort (=abort back)
          ELSEIF lv_answer = lc_answer_cancel. " abort
            " do nothing
          ENDIF.
* if there are no changes, leave without saving changes
        ELSE. " no save required
          me->end_processing( ).
          LEAVE TO SCREEN 0.
*!!!            lcl_selection_screen=>call_selection_screen( iv_objnr = gv_objnr ).

        ENDIF.

* EXIT
      WHEN gc_ucomm_exit.
* if there had been changes, show popup
        IF gv_save_needed = abap_true.
          CALL FUNCTION 'POPUP_TO_CONFIRM_DATA_LOSS'
            EXPORTING
              titel  = text-pop
            IMPORTING
              answer = lv_answer.
* save changes if user pressed on save yes
          IF lv_answer = lc_answer_yes.
            me->save_changes( ).
            me->end_processing( ).
            LEAVE TO SCREEN 0100.
* do not save if user pressed on no save
          ELSEIF lv_answer EQ lc_answer_no.
            me->end_processing( ).
            LEAVE TO SCREEN 0100.
* do nothing, if user press on abort (=abort back)
          ELSEIF lv_answer = lc_answer_cancel. " abort
            " do nothing
          ENDIF.
* if there are no changes, leave without saving changes
        ELSE. " no save required
          me->end_processing( ).
          LEAVE TO SCREEN 0100.
        ENDIF.

* CANC (=Cancel)
      WHEN gc_ucomm_canc.
* leave without saving changes
        me->end_processing( ).
        LEAVE TO SCREEN 0100.

* STORNO
      WHEN gc_ucomm_storno.
* mark selected documents for cancel
        me->cancel_documents( ).

* SAVE
      WHEN gc_ucomm_save.
* save changes (cancel marked IFCU measurements)
        me->save_changes( ).

      WHEN tzs1_fcode_button.
*--- time zone button
        BREAK-POINT ID eam_tzs_gen.

        IF gv_time_zone_active = tzs1_active-yes.
*--- only if time zone support is active
          CALL BADI gb_badi_time_zone_generic->time_zone_button_on_list.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "execute_user_command_tree
************************************************************************
* Module  END_PROCESSING
*-----------------------------------------------------------------------
* Purpose: clears global data and tree
************************************************************************
  METHOD end_processing.
* data declaration
    DATA: lo_lcl_documents_data TYPE REF TO lcl_documents_data.

* main part ------------------------------------------------------------
* if the tree for output of the IFCU measurements is initialized, free it
    IF go_tree IS BOUND.
      go_tree->free( ).
    ENDIF.

* clear global variables
    CLEAR:
       go_tree,
       gs_icon,
       gv_label_mdoc,
       gv_datee,
       gv_dates,
       gv_save_needed.

* refresh global tables
    REFRESH:
       gt_docs_reverse,
       gt_tree_tab,
       gt_tree_output,
       gt_docs.

* free data in class for data source
    lo_lcl_documents_data = lcl_documents_data=>get_reference( ).
    lo_lcl_documents_data->end_processing( ).

  ENDMETHOD.                    "end_processing
************************************************************************
* Module  GET_SELECTED_NODES
*-----------------------------------------------------------------------
* Purpose: get selected nodes from tree (selected IFCU measurements)
*************************************************************************
  METHOD get_selected_nodes.
* data declaration
    DATA: lv_node TYPE lvc_nkey.

* main part ------------------------------------------------------------
* clear exporting parameter, to avoid mistakes with filled, old data
    CLEAR: et_nodes.

* get selected nodes of the tree
    go_tree->get_selected_nodes(
    CHANGING ct_selected_nodes = et_nodes ).

* if no node selected -> take node of selected item
    IF et_nodes IS INITIAL.
      go_tree->get_selected_item( IMPORTING e_selected_node = lv_node ).
      IF lv_node IS NOT INITIAL.
        APPEND lv_node TO et_nodes.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "get_selected_nodes
************************************************************************
* Module  CANCEL_DOCUMENTS
*-----------------------------------------------------------------------
* Purpose: mark IFCU measurement and goods movement documents for cancel
*************************************************************************
  METHOD cancel_documents.
* data declaration
    DATA:
          lt_nodes                  TYPE lvc_t_nkey,
          lt_nodes_sel              TYPE lvc_t_nkey.

    FIELD-SYMBOLS: <ls_node>        LIKE LINE OF lt_nodes,
                   <ls_tree_output> LIKE LINE OF gt_tree_output,
                   <ls_docs_tmp>    LIKE LINE OF gt_docs.

* main part ------------------------------------------------------------
* get selected nodes of tree
    me->get_selected_nodes( IMPORTING et_nodes = lt_nodes_sel ).

* at least one valid entry have to be selected
    IF lt_nodes_sel IS INITIAL.
      MESSAGE e025(ih).
      RETURN.
    ENDIF.

* for all selected nodes do the following:
    LOOP AT lt_nodes_sel ASSIGNING <ls_node>.

* read tree data of selected node
      READ TABLE gt_tree_output ASSIGNING <ls_tree_output>
                                WITH TABLE KEY node = <ls_node>.
      IF sy-subrc NE 0.
* should never happen
        CONTINUE.
      ENDIF.

* Only root nodes can be selected, check again, if selected node is a root node
      IF <ls_tree_output>-parent_node IS NOT INITIAL.
* Error, if selected node is not a root node
        MESSAGE s059(itobfltcon).
* Root node was selected
      ELSE.
* copy document numbers of global table in table for reverse of documents
        LOOP AT gt_docs  ASSIGNING <ls_docs_tmp>                            " "#EC CI_NESTED
                         WHERE meas_date = <ls_tree_output>-data-meas_date
                         AND   meas_time = <ls_tree_output>-data-meas_time.

* If selected node was already marked for cancel, unmark reverse flag
          IF <ls_docs_tmp>-rev_flag IS NOT INITIAL.
* unmark flag in global data and in output
            CLEAR <ls_docs_tmp>-rev_flag.
            CLEAR <ls_tree_output>-data-rev_flag.
* delete document from table, with documents which should be cancelled
            DELETE TABLE gt_docs_reverse WITH TABLE KEY meas_date = <ls_docs_tmp>-meas_date
                                                        meas_time = <ls_docs_tmp>-meas_time.

* if node was not marked for cancel, mark the node for cancel
          ELSE.
* mark flag in global data and in output
            <ls_docs_tmp>-rev_flag = gs_icon-storno.
            <ls_tree_output>-data-rev_flag = gs_icon-storno.
* append data to global table, where all documents to cancel are stored
            INSERT <ls_docs_tmp> INTO TABLE gt_docs_reverse.
          ENDIF.
        ENDLOOP.
      ENDIF.

* change node (display mark for deletion or remove it)
      CALL METHOD go_tree->change_node
        EXPORTING
          i_node_key     = <ls_node>
          i_outtab_line  = <ls_tree_output>-data
        EXCEPTIONS
          node_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* this should never happen
        MESSAGE a053(itobfltcon).
      ENDIF.

* update frontend, that changes are visible
      go_tree->frontend_update( ).
    ENDLOOP.

* as there had been changes, set flag that changes occured
    gv_save_needed = abap_true.

  ENDMETHOD.                    "cancel_documents
************************************************************************
* Module  SAVE_CHANGES
*-----------------------------------------------------------------------
* Purpose: save changes (cancel IFCU measurements)
*************************************************************************
  METHOD save_changes.
* data declaration
    DATA:
          lv_temp_date               TYPE  gty_ifcu_doc_data-meas_date,
          lv_temp_time               TYPE  gty_ifcu_doc_data-meas_time,
          lv_temp_date_old           TYPE  gty_ifcu_doc_data-meas_date,
          lv_temp_time_old           TYPE  gty_ifcu_doc_data-meas_time,
          lv_message                 TYPE c LENGTH 1,       "#EC NEEDED
          lv_log_header              TYPE bal_s_log,
          lv_log_handle              TYPE balloghndl,"log handle
          ls_msg_obj                 TYPE bal_s_msg, " general message with document information
          lv_mblnr                   TYPE bapi2017_gm_head_02-mat_doc,
          lv_mjahr                   TYPE bapi2017_gm_head_02-doc_year,
          lt_log_handle_disp         TYPE bal_t_logh,
          ls_display_profile         TYPE bal_s_prof,
          lt_docs_mdoc_reverse       TYPE TABLE OF imrg_mdocm.

    FIELD-SYMBOLS: <ls_docs_tmp>     LIKE LINE OF gt_docs.

    CONSTANTS: lc_doc_type_m         TYPE eam_ifcu_document_type VALUE 'M',
               lc_doc_type_g         TYPE eam_ifcu_document_type VALUE 'G'.

* initialize application log
    lv_log_header-aldate    = sy-datum.
    lv_log_header-altime    = sy-uzeit.
    lv_log_header-aluser    = sy-uname.
*******************************************************************************
* start logging / preparation before processing is started
*******************************************************************************
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = lv_log_header
      IMPORTING
        e_log_handle            = lv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

* for display, set handle value for the log in local table (is used later)
    APPEND lv_log_handle TO lt_log_handle_disp.

* first check if at least one document was selected
    IF gt_docs_reverse IS INITIAL.
* message that no documents to cnacel are selected
      MESSAGE e061(itobfltcon) INTO lv_message.
* message with data, that measurement documents causes problems and which date/time
      ls_msg_obj-msgty = sy-msgty.
      ls_msg_obj-msgid = sy-msgid.
      ls_msg_obj-msgno = sy-msgno.
      ls_msg_obj-msgv1 = sy-msgv1.
      ls_msg_obj-msgv2 = sy-msgv2.
      ls_msg_obj-msgv3 = sy-msgv3.
      ls_msg_obj-msgv4 = sy-msgv4.

* general message with information which reverse data is meant (time/date)
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = lv_log_handle
          i_s_msg          = ls_msg_obj
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e055(itobfltcon).
        RETURN.
      ENDIF.
      RETURN. " leave function if there are no documents selected
    ENDIF.

* for all documents in the global table to cancel documents do the following:
    LOOP AT gt_docs_reverse ASSIGNING <ls_docs_tmp>.
* remember current time/date combination in local variables
      MOVE <ls_docs_tmp>-meas_date TO lv_temp_date.
      MOVE <ls_docs_tmp>-meas_time TO lv_temp_time.

*********************************************************************************************
* All documents of same date/time are read in local variables
* -> Reverse documents
* Remember: all documents of a specific time/date combination are cancelled or none!
*********************************************************************************************
* check if the step before was the same date/time like this one
      IF ( lv_temp_date_old IS NOT INITIAL AND
          lv_temp_time_old IS NOT INITIAL ) AND
         ( lv_temp_date_old NE lv_temp_date OR
           lv_temp_time_old NE lv_temp_time ).
**********************************************************************************************
** Cancel Measurement documents & Goods movement documents
**********************************************************************************************
        me->cancel_doc_at_save(
              EXPORTING
                iv_doc_date          = lv_temp_date_old
                iv_doc_time          = lv_temp_time_old
                iv_log_handle        = lv_log_handle
                iv_mblnr             = lv_mblnr
                iv_mjahr             = lv_mjahr
              CHANGING
                ct_docs_mdoc_reverse = lt_docs_mdoc_reverse ).
*
*****************************************************************************************
* Clear local variables for next step
*****************************************************************************************
* clear local variables, for next loop step
        REFRESH:
          lt_docs_mdoc_reverse.

        CLEAR:
          lv_mblnr,
          lv_mjahr.
      ENDIF.

* set current document as new date/time old reference
      lv_temp_date_old = lv_temp_date.
      lv_temp_time_old = lv_temp_time.

******************************************************************************
* prepare cancel documents for the selected date / time
******************************************************************************
* Add documents to local table. This is required, as all or nothing
* needs to be written on the database. The documents are collected in each loop step
* and then the commit work or rollback work is done when all documents are in the local
* table
      IF <ls_docs_tmp>-doc_type EQ lc_doc_type_m.
        APPEND <ls_docs_tmp>-number TO lt_docs_mdoc_reverse.
      ELSEIF  <ls_docs_tmp>-doc_type EQ lc_doc_type_g.
        lv_mblnr = <ls_docs_tmp>-number.
        lv_mjahr = <ls_docs_tmp>-doc_year.
      ENDIF.
    ENDLOOP. " end of loopstep for each document which should be cancelled

* after loop the documents of the last date/time combination are not cancelled.
* This is done now
    me->cancel_doc_at_save(
          EXPORTING
            iv_doc_date          = lv_temp_date
            iv_doc_time          = lv_temp_time
            iv_log_handle        = lv_log_handle
            iv_mblnr             = lv_mblnr
            iv_mjahr             = lv_mjahr
          CHANGING
            ct_docs_mdoc_reverse = lt_docs_mdoc_reverse ).

* After all documents are cancelled show action log with errors (if occured)
    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile
      EXCEPTIONS
        OTHERS              = 99.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE e055(itobfltcon).
    ENDIF.
    ls_display_profile-use_grid  = abap_true.
    ls_display_profile-disvariant-handle = 'LOG'.
* display all messages from log

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = ls_display_profile
        i_t_log_handle       = lt_log_handle_disp
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
* should not happen
      MESSAGE e055(itobfltcon).
    ENDIF.

* after reverse of all documents, clear the global "change" flag
    gv_save_needed = abap_false.

  ENDMETHOD.                    " save_changes
************************************************************************
* Module  HANDLE_DOUBLE_CLICK_TREE
*-----------------------------------------------------------------------
* Purpose: Double click on Tree
************************************************************************
  METHOD handle_double_click_tree.
* data declaration
    FIELD-SYMBOLS: <ls_tree_output> LIKE LINE OF gt_tree_output.

    CONSTANTS: lc_parameter_measdoc   TYPE c VALUE 'IMD'  LENGTH 3,
               lc_trans_measdoc       TYPE c VALUE 'IK13' LENGTH 4,
               lc_parameter_goodsdoc  TYPE c VALUE 'MBN'  LENGTH 3,
               lc_parameter_goodsyear TYPE c VALUE 'MJA'  LENGTH 3,
               lc_trans_goodsdoc      TYPE c VALUE 'MB03' LENGTH 4.

* go to transaction, if double click on measurement/goods movement document
    CASE fieldname.
      WHEN 'NUMBER'.
* read detail data of selected document
        READ TABLE gt_tree_output ASSIGNING <ls_tree_output>
                                  WITH TABLE KEY node = node_key.
        IF sy-subrc IS NOT INITIAL.
* this should never happen
          MESSAGE a053(itobfltcon).
        ELSE.
* depending on the document type, go to corresponding transaction
* measurement document
          IF <ls_tree_output>-data-doc_type EQ 'M'.
            SET PARAMETER ID lc_parameter_measdoc FIELD <ls_tree_output>-data-number.
            CALL TRANSACTION lc_trans_measdoc AND SKIP FIRST SCREEN.
* goods movement document
          ELSEIF <ls_tree_output>-data-doc_type EQ 'G'.
            SET PARAMETER ID lc_parameter_goodsdoc  FIELD <ls_tree_output>-data-number.
            SET PARAMETER ID lc_parameter_goodsyear FIELD <ls_tree_output>-data-doc_year.
            CALL TRANSACTION lc_trans_goodsdoc AND SKIP FIRST SCREEN.
          ENDIF. " jump in document transaction
*--- set time zone back to list time zone
          lcl_selection_screen=>set_time_zone_back( ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.                    " handle_double_click_tree
************************************************************************
* Module  CANCEL_AT_SAVE
*-----------------------------------------------------------------------
* Purpose: Cancel documents at save
************************************************************************
  METHOD cancel_doc_at_save.
* cancel measurement document and goods movement of one date/time combination

* data declaration
    DATA: lv_message       TYPE c LENGTH 1,                 "#EC NEEDED
          lv_equnr         TYPE equi-equnr,
          ls_msg           TYPE bal_s_msg,
          ls_msg_obj       TYPE bal_s_msg,
          lv_error_occured TYPE flag,
          lt_return        TYPE bapirettab,
          ls_eam_ifcu      TYPE eam_ifcu,
          lt_eam_ifcu      TYPE TABLE OF eam_ifcu.

    FIELD-SYMBOLS: <ls_return>       TYPE bapiret2,
                   <ls_tree_output>  LIKE LINE OF gt_tree_output.

    CONSTANTS:
              lc_error_e            TYPE sy-msgty               VALUE 'E',
              lc_error_a            TYPE sy-msgty               VALUE 'A',
              lc_error_x            TYPE sy-msgty               VALUE 'X'.
*********************************************************************************************
* Cancel Measurement documents
*********************************************************************************************
* cancel all measurement documents of the time/date combination
    CALL FUNCTION 'MEASUREM_DOCUM_CANCEL_ARRAY'
      EXPORTING
        messages_allowed       = abap_false
        prepare_update         = abap_false
      TABLES
        cancel_requests        = ct_docs_mdoc_reverse
      EXCEPTIONS
        no_authority           = 1
        foreign_lock_occured   = 2
        system_failure_occured = 3
        recursiveness_found    = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
* store sy message in local variable. later the message will be insert in action log, but a general message
* will be inserted in action log before, so the message is stored before
      ls_msg-msgty = sy-msgty.
      ls_msg-msgid = sy-msgid.
      ls_msg-msgno = sy-msgno.
      ls_msg-msgv1 = sy-msgv1.
      ls_msg-msgv2 = sy-msgv2.
      ls_msg-msgv3 = sy-msgv3.
      ls_msg-msgv4 = sy-msgv4.

* general message for action log, that messages occured while cancelling measurement documents
      MESSAGE e056(itobfltcon) WITH iv_doc_date iv_doc_time INTO lv_message.
* message with data, that measurement documents causes problems and which date/time
      ls_msg_obj-msgty = sy-msgty.
      ls_msg_obj-msgid = sy-msgid.
      ls_msg_obj-msgno = sy-msgno.
      ls_msg_obj-msgv1 = sy-msgv1.
      ls_msg_obj-msgv2 = sy-msgv2.
      ls_msg_obj-msgv3 = sy-msgv3.
      ls_msg_obj-msgv4 = sy-msgv4.

* Add both message to log
* general message with information which reverse data is meant (time/date)
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = iv_log_handle
          i_s_msg          = ls_msg_obj
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e055(itobfltcon).
        RETURN.
      ENDIF.

* message from cancel measurement document
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = iv_log_handle
          i_s_msg          = ls_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e055(itobfltcon).
        RETURN.
      ENDIF.

* if the message was an error message, then set flag that an error occured
      lv_error_occured = abap_true.
    ENDIF. " error handling while cancel of measurement documents

********************************************************************************************
* Cancel goods movement document
********************************************************************************************
* Only if no error occured for the measurement documents, do the goods movement cancel
* Remember -> all or nothing
    IF lv_error_occured IS INITIAL AND
       iv_mblnr IS NOT INITIAL AND
       iv_mjahr IS NOT INITIAL.
* cancel goods movement document
      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument = iv_mblnr
          matdocumentyear  = iv_mjahr
          goodsmvt_pstng_date = iv_doc_date                  "n 2336866
        TABLES
          return           = lt_return.
      IF lt_return IS NOT INITIAL.
* if lt_return is not initial, create message with data about the goods movement
        MESSAGE e057(itobfltcon) WITH iv_doc_date iv_doc_time INTO lv_message.
* message with data of goods movement error
        ls_msg_obj-msgty = sy-msgty.
        ls_msg_obj-msgid = sy-msgid.
        ls_msg_obj-msgno = sy-msgno.
        ls_msg_obj-msgv1 = sy-msgv1.
        ls_msg_obj-msgv2 = sy-msgv2.
        ls_msg_obj-msgv3 = sy-msgv3.
        ls_msg_obj-msgv4 = sy-msgv4.

* Add message to log
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle     = iv_log_handle
            i_s_msg          = ls_msg_obj
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
        IF sy-subrc IS NOT INITIAL.
          MESSAGE e055(itobfltcon).
          RETURN.
        ENDIF.

* All messages will be transferred to action log
        LOOP AT lt_return ASSIGNING <ls_return>.         "#EC CI_NESTED
          ls_msg-msgty = <ls_return>-type.
          ls_msg-msgid = <ls_return>-id.
          ls_msg-msgno = <ls_return>-number.
          ls_msg-msgv1 = <ls_return>-message_v1.
          ls_msg-msgv2 = <ls_return>-message_v2.
          ls_msg-msgv3 = <ls_return>-message_v3.
          ls_msg-msgv4 = <ls_return>-message_v4.

* Add message to log
          CALL FUNCTION 'BAL_LOG_MSG_ADD'
            EXPORTING
              i_log_handle     = iv_log_handle
              i_s_msg          = ls_msg
            EXCEPTIONS
              log_not_found    = 1
              msg_inconsistent = 2
              log_is_full      = 3
              OTHERS           = 4.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE e055(itobfltcon).
            RETURN.
          ENDIF.
* if an error occured, set local variables that error occured
          IF <ls_return>-type EQ lc_error_e OR
             <ls_return>-type EQ lc_error_a OR
             <ls_return>-type EQ lc_error_x.
            lv_error_occured = abap_true.
          ENDIF.
        ENDLOOP.
      ENDIF. " if there are messages from cancel goods movement

* if an error occured go to next loopstep
      IF lv_error_occured IS INITIAL.
* If no error occured, remove entry of goods movement in database table EAM_IFCU
        MOVE iv_mjahr      TO ls_eam_ifcu-mjahr.
        MOVE iv_mblnr      TO ls_eam_ifcu-mblnr.
        MOVE gv_objnr      TO ls_eam_ifcu-objnr.
        MOVE iv_doc_date   TO ls_eam_ifcu-posting_date.
        MOVE iv_doc_time   TO ls_eam_ifcu-posting_time.
        APPEND ls_eam_ifcu TO lt_eam_ifcu.

* remove the db-table entry in EAM_IFCU
        CALL FUNCTION 'EAM_IFCU_DELETE' IN UPDATE TASK
          TABLES
            it_eam_ifcu = lt_eam_ifcu.
* clear variables for next loopstep
        CLEAR ls_eam_ifcu.
        REFRESH lt_eam_ifcu.
      ENDIF.
    ENDIF.
*****************************************************************************************
* if there are no errors before Commit work and update the display
*****************************************************************************************
* if an error occured go to next loopstep
    IF lv_error_occured IS INITIAL.
* prepare to save the buffer changes
      CALL FUNCTION 'MEASUREM_DIALOG_UPDATE'.
* make changes permanent, using commit work and wait
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

* delete entries in global table for documents which are already cancelled
      DELETE gt_docs_reverse WHERE meas_date = iv_doc_date
                             AND   meas_time = iv_doc_time.

      DELETE gt_docs         WHERE meas_date = iv_doc_date
                             AND   meas_time = iv_doc_time.

* Update alv-tree and remove subtree
      READ TABLE gt_tree_output ASSIGNING <ls_tree_output> "#EC CI_HASHSEQ
                                WITH KEY data-meas_date = iv_doc_date
                                         data-meas_time = iv_doc_time.

      go_tree->delete_subtree( i_node_key                = <ls_tree_output>-node
                               i_update_parents_expander = abap_true
                               i_update_parents_folder   = abap_true ).

      go_tree->frontend_update( ).


* if there are no errors, insert success message
* general message for action log, that messages occured while cancelling measurement documents
      MESSAGE s060(itobfltcon) WITH iv_doc_date iv_doc_time INTO lv_message.
* message with data, that measurement documents causes problems and which date/time
      ls_msg_obj-msgty = sy-msgty.
      ls_msg_obj-msgid = sy-msgid.
      ls_msg_obj-msgno = sy-msgno.
      ls_msg_obj-msgv1 = sy-msgv1.
      ls_msg_obj-msgv2 = sy-msgv2.
      ls_msg_obj-msgv3 = sy-msgv3.
      ls_msg_obj-msgv4 = sy-msgv4.

* Add message to log
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = iv_log_handle
          i_s_msg          = ls_msg_obj
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e055(itobfltcon).
        RETURN.
      ENDIF.
    ENDIF.

*****************************************************************************************
* handling if an error occured
*****************************************************************************************
    IF lv_error_occured IS NOT INITIAL.
* rollback the cancel of the measurement documents
      CALL FUNCTION 'MEASUREM_DIALOG_CANCEL'.

* if an error occured, and all related documents are skipped, rollback work
* to guarantee no inconsistencies on database
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.


* free ITOB Buffer before locking again the equipment, as there can be problems with the
* ITOB buffer in some cases
    CALL FUNCTION 'ITOB_BUF_FREE'
      EXCEPTIONS
        not_successful = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* this should not occur
      MESSAGE e053(itobfltcon).
    ENDIF.

* get Equipment Number from Object number
    MOVE gv_objnr+2 TO lv_equnr.

* lock equipment, as commit work and rollback work removes the lock of the equipment
    CALL FUNCTION 'EQUIPMENT_LOCK'
      EXPORTING
        equi_no        = lv_equnr
      EXCEPTIONS
        equi_not_found = 1
        lock_failure   = 2
        OTHERS         = 3.

  ENDMETHOD.                    " cancel_doc_at_save

ENDCLASS. " lcl_documents_output
