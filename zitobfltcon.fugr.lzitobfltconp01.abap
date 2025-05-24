*&---------------------------------------------------------------------*
*&  Include           LITOBFLTCONP01
*&---------------------------------------------------------------------*
CLASS lcl_selection_screen IMPLEMENTATION.

************************************************************************
* Module  CALL_SELECTION_SCREEN
*-----------------------------------------------------------------------
* Purpose: call selection screen
************************************************************************
  METHOD call_selection_screen.

* data declaration
    TYPES: lty_imrg    TYPE          imrg,
           lty_t_imrg  TYPE STANDARD TABLE OF lty_imrg,
           lty_impt    TYPE          impt,
           lty_t_impt  TYPE STANDARD TABLE OF lty_impt,
           lty_godsm   TYPE          eam_ifcu,
           lty_t_godsm TYPE STANDARD TABLE OF lty_godsm.

    DATA:
          lt_output               TYPE lcl_documents_output=>gty_t_ifcu_doc_data,
          ls_output               LIKE LINE OF lt_output,
          lt_imrg                 TYPE lty_t_imrg,
          lt_impt                 TYPE SORTED TABLE OF impt WITH UNIQUE KEY point,
          lt_godsm                TYPE lty_t_godsm,
          lt_return               TYPE bapirettab,
          lo_lcl_documents_data   TYPE REF TO lcl_documents_data,
          lo_lcl_documents_output TYPE REF TO lcl_documents_output,
          lv_variant              TYPE rsvar-variant,
          lv_dates                TYPE d,
          lv_datee                TYPE d,
          lv_times                TYPE t VALUE '000000',
          lv_timee                TYPE t VALUE '235959'.

    FIELD-SYMBOLS:
           <ls_impt>              TYPE impt,
           <ls_imrg>              TYPE imrg,
           <ls_godsm>             TYPE eam_ifcu,
           <ls_return>            TYPE bapiret2.

    CONSTANTS:
          lc_doc_type_m           TYPE eam_ifcu_document_type VALUE 'M', " type for measurement docs
          lc_doc_type_g           TYPE eam_ifcu_document_type VALUE 'G', " type for goods movement
          lc_trans_ifcu           TYPE sy-tcode               VALUE 'IFCU',
          lc_reportid             TYPE rsvar-report           VALUE 'SAPLITOBFLTCON'.
* end of data declaration

* read ITOB for object and fill global variable ITOB with ITOB data
* -- for equipment
    IF itob-equnr IS NOT INITIAL.
      CALL FUNCTION 'ITOB_EQUIPMENT_READ_SINGLE'
        EXPORTING
          i_objnr        = itob-equnr
        IMPORTING
          e_object_rec   = itob
        EXCEPTIONS
          not_successful = 1
          OTHERS         = 2.
* this should not happen. Existence check of Equipment is done in screen 0100
* and must be calles before
      IF sy-subrc <> 0.
        MESSAGE e053(itobfltcon).
      ENDIF.
* lock equipment
      CALL FUNCTION 'EQUIPMENT_LOCK'
        EXPORTING
          equi_no        = itob-equnr
        EXCEPTIONS
          equi_not_found = 1
          lock_failure   = 2
          OTHERS         = 3.
* This should not happen
      IF sy-subrc <> 0 AND
         sy-subrc NE 2.
        MESSAGE e053(itobfltcon).
      ELSEIF sy-subrc EQ 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

* set name of default variant in local variable
    lv_variant = 'U_'.
    WRITE sy-uname TO lv_variant+2.

* get default variant, if available
    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING
        report  = lc_reportid
        variant = lv_variant
      EXCEPTIONS
        OTHERS  = 99.
* if default variant exists, set it
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
        EXPORTING
          report               = lc_reportid
          variant              = lv_variant
        EXCEPTIONS
          variant_not_existent = 01
          variant_obsolete     = 02.
    ENDIF.

    DO.
* refresh variable with data, to avoid errors or double entries from run before
      REFRESH lt_output.
* call selection screen
      CALL SELECTION-SCREEN 0207.

* selection screen was cancelled
      IF sy-subrc NE 0.
* unlock equipment
        IF itob-equnr IS NOT INITIAL.
* After completing processing, unlock equipment
          CALL FUNCTION 'EQUIPMENT_UNLOCK'
            EXPORTING
              equi_no        = itob-equnr
            EXCEPTIONS
              equi_not_found = 1
              lock_failure   = 2
              OTHERS         = 3.
* This should not happen
          IF sy-subrc <> 0.
            MESSAGE e053(itobfltcon).
          ENDIF.
        ENDIF.

        LEAVE TO TRANSACTION lc_trans_ifcu.
      ENDIF.

*--- for time zone support convert dates
      lv_dates = sp_dates.
      lv_datee = sp_datee.

      convert_sel_opt_to_syst(
        CHANGING
          cv_date_from = lv_dates
          cv_date_to   = lv_datee
          cv_time_from = lv_times
          cv_time_to   = lv_timee ).

* get required documents for technical object
      lo_lcl_documents_data = lcl_documents_data=>get_reference( ).

      lo_lcl_documents_data->get_documents(
                               EXPORTING
                                 iv_objnr  = itob-objnr
                                 iv_dates  = lv_dates
                                 iv_datee  = lv_datee
                                 iv_times  = lv_times
                                 iv_timee  = lv_timee
                                 iv_ifcuo  = sp_ifcuo
                               IMPORTING
                                 et_imrg   = lt_imrg
                                 et_impt   = lt_impt
                                 et_godsm  = lt_godsm
                                 et_return = lt_return ).

* return error if one occured
      LOOP AT lt_return ASSIGNING <ls_return>            "#EC CI_NESTED
                        WHERE type EQ 'A'
                        OR    type EQ 'E'
                        OR    type EQ 'X'.
        MESSAGE ID <ls_return>-id TYPE <ls_return>-type NUMBER <ls_return>-number
        WITH <ls_return>-message_v1 <ls_return>-message_v2 <ls_return>-message_v3 <ls_return>-message_v4.
      ENDLOOP.

* get required reference of class for the output of the documents of a techn. object
      lo_lcl_documents_output = lcl_documents_output=>get_reference( ).

***************************************************************************
* Prepare output for measurement documents
***************************************************************************
* prepare measurement documents data for output
      LOOP AT lt_imrg ASSIGNING <ls_imrg>.               "#EC CI_NESTED
* move type 'm' for measurement documents to output structure
        MOVE lc_doc_type_m      TO ls_output-doc_type.
        MOVE <ls_imrg>-mdocm    TO ls_output-number.

* read the measurement position and text
        READ TABLE lt_impt WITH TABLE KEY point = <ls_imrg>-point
                           ASSIGNING      <ls_impt>.
        IF sy-subrc IS NOT INITIAL.
* error, this should not happen
          MESSAGE e053(itobfltcon).
        ENDIF.

* fill output structure with read information from before
        MOVE <ls_impt>-psort    TO ls_output-position.
        MOVE <ls_impt>-pttxt    TO ls_output-pos_txt.

* get the measurement value of the measurement document
        IF <ls_imrg>-idiff IS NOT INITIAL.
* if value was entered using difference, then move difference value to output structure
          MOVE <ls_imrg>-cdiff TO ls_output-value.

          CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
            EXPORTING
              char_unit       = <ls_imrg>-recdu
              decimals        = <ls_impt>-decim
              exponent        = <ls_impt>-expon
              fltp_value_si   = <ls_imrg>-cdiff
              indicator_value = abap_true
            IMPORTING
              char_value      = ls_output-value
            EXCEPTIONS
              no_unit_given   = 1
              unit_not_found  = 2
              OTHERS          = 3.
          IF sy-subrc IS NOT INITIAL.
* error, this should not happen
            MESSAGE e053(itobfltcon).
          ENDIF.
        ELSE.
* if value was entered using reading value, move counter reading value to output structure
          MOVE <ls_imrg>-cntrr TO ls_output-value.
* convert output value
          CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
            EXPORTING
              char_unit       = <ls_imrg>-recdu
              decimals        = <ls_impt>-decim
              exponent        = <ls_impt>-expon
              fltp_value_si   = <ls_imrg>-cntrr
              indicator_value = abap_true
            IMPORTING
              char_value      = ls_output-value
            EXCEPTIONS
              no_unit_given   = 1
              unit_not_found  = 2
              OTHERS          = 3.
          IF sy-subrc IS NOT INITIAL.
* error, this should not happen
            MESSAGE e053(itobfltcon).
          ENDIF.
        ENDIF.

* complete the filling of the output structure
* move measurement unit / date / time
        MOVE <ls_imrg>-recdu   TO ls_output-meas_unit.
        MOVE <ls_imrg>-idate   TO ls_output-meas_date.
        MOVE <ls_imrg>-itime   TO ls_output-meas_time.

*--- convert to time zone
        convert_output_to_sess(
          CHANGING
            cs_output = ls_output ).

        APPEND ls_output TO lt_output.
      ENDLOOP.

* clear old values in ls_output, that there are no old values in the structure
* when goods movement documents are prepared for output
      CLEAR ls_output.

***************************************************************************
* Prepare output for goods movement documents
***************************************************************************
* Store Goods Movement data in global table, as this data is needed later
* for cancel of the goods movement
      gt_godsm[] = lt_godsm[].


      LOOP AT lt_godsm ASSIGNING <ls_godsm>.             "#EC CI_NESTED
* move type g for goods movement
        MOVE lc_doc_type_g           TO ls_output-doc_type.
* fill output structure
        MOVE <ls_godsm>-mblnr        TO ls_output-number.
        MOVE <ls_godsm>-mjahr        TO ls_output-doc_year.
        MOVE <ls_godsm>-posting_date TO ls_output-meas_date.
        MOVE <ls_godsm>-posting_time TO ls_output-meas_time.

*--- convert to time zone
        convert_output_to_sess(
          CHANGING
            cs_output = ls_output ).

        APPEND ls_output             TO lt_output.
      ENDLOOP.

***************************************************************************
* Prepare documents (goods and measurements)
***************************************************************************
* sort table for correct order
      SORT lt_output BY meas_date DESCENDING
                        meas_time DESCENDING
                        doc_type  DESCENDING AS TEXT
                        position  ASCENDING  AS TEXT.

* only process screen, if there is at least one entry selected in timeframe
      IF lt_output IS NOT INITIAL.

* get required reference of class for output of the documents of a techn. object
        lo_lcl_documents_output->call_screen( EXPORTING
                                                iv_objnr = itob-objnr
                                                it_docs  = lt_output
                                                iv_datee = sp_datee
                                                iv_dates = sp_dates ).

      ELSE.
        MESSAGE s058(itobfltcon).
      ENDIF.
    ENDDO.
  ENDMETHOD.   " call_selection_screen.
************************************************************************
* Module  AT_SELECTION_SCREEN
*-----------------------------------------------------------------------
* Purpose: check input at selection screen
************************************************************************
  METHOD at_selection_screen.
* check that end date is not lower than start date.
    IF sp_dates GT sp_datee.
      MESSAGE i051(itobfltcon).
    ENDIF.

*--- time zone function
    IF sscrfields-ucomm = 'FC01'.
      PERFORM time_zone_button_pressed.
      CLEAR sscrfields-ucomm.
    ENDIF.

  ENDMETHOD.   " at_selection_screen

************************************************************************
* Module  AT_SELECTION_SCREEN_OUTPUT
*-----------------------------------------------------------------------
* Purpose: check input at selection screen
************************************************************************
  METHOD at_selection_screen_output.

*--- create time zone button
    create_time_zone_button( ).

  ENDMETHOD.                    "at_selection_screen_output


  INCLUDE litobfltconp04. " methods for time zone support

ENDCLASS. " lcl_selection_screen
