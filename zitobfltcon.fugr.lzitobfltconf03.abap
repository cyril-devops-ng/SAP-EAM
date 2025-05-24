*----------------------------------------------------------------------*
***INCLUDE LITOBFLTCONF03 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_T370CON_TOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_t370con_tol  TABLES   pt_impt          STRUCTURE impt
                        USING    p_itob           TYPE itob
                                 p_fleet          TYPE fleet
                                 p_count_cdiff    LIKE imrg-cdiff
                                 p_pri_count_pt   TYPE imrc_point
                                 p_count_cntrr    TYPE imrc_cntrr
                                 p_count_decim    TYPE imrc_decim
                                 p_count_unit     TYPE imrc_recdu
                                 p_consp_cdiff    LIKE imrg-cdiff
                                 p_pri_consp_pt   TYPE imrc_point
                        CHANGING p_subrc          LIKE sy-subrc
                                 p_value_min_char LIKE rimr0-readc
                                 p_value_max_char LIKE rimr0-readc.

  DATA:
    l_value_num  LIKE rimr01-recdv,
    l_value_char LIKE rimr0-readc,
    l_value_min  LIKE rimr01-recdv,
    l_value_max  LIKE rimr01-recdv,
    t_imptt      LIKE imptt OCCURS 0 WITH HEADER LINE.

*-> check readings not equal 0
  IF p_count_cdiff IS INITIAL OR
     p_consp_cdiff IS INITIAL.
    p_subrc = 1.
    EXIT.
  ENDIF.
*-> fill points for call FLEET_GET_CONSUMP_INFO
  READ TABLE pt_impt WITH KEY point = p_pri_consp_pt.
  IF sy-subrc <> 0.
    p_subrc = 1.
    EXIT.
  ENDIF.
  MOVE-CORRESPONDING pt_impt TO t_imptt.
  APPEND t_imptt.
  READ TABLE pt_impt WITH KEY point = p_pri_count_pt.
  IF sy-subrc <> 0.
    p_subrc = 1.
    EXIT.
  ENDIF.
  MOVE-CORRESPONDING pt_impt TO t_imptt.
  APPEND t_imptt.
*-> ensure T370CLC is correct
  IF t370clc-calc_key <> p_fleet-pri_calc.
    SELECT SINGLE * FROM t370clc INTO t370clc
           WHERE  calc_key = p_fleet-pri_calc.
    IF sy-subrc <> 0.
      t370clc-calc_key = p_fleet-pri_calc.
    ENDIF.
  ENDIF.
  CLEAR fleet_info.
  CALL FUNCTION 'FLEET_GET_CONSUMP_INFO'
    EXPORTING
      i_itob                  = p_itob
      i_t370clc               = t370clc
      i_fleet                 = p_fleet
    TABLES
      itab_imptt              = t_imptt
    CHANGING
      c_fleet_info            = fleet_info
      e_code                  = p_subrc
    EXCEPTIONS
      OTHERS                  = 1.
  IF fleet_info-consum_s IS INITIAL AND
     fleet_info-consum_l IS INITIAL.
    IF sy-subrc = 0.
      p_subrc = 5.                     "not enough measurments
    ELSE.
      p_subrc = 1.
    ENDIF.
    EXIT.
  ENDIF.
  CLEAR p_subrc.
*-> all data available -> we can check the rule ---------------
  IF NOT t370con_tol-avg_short IS INITIAL AND
     NOT fleet_info-consum_s IS INITIAL.
    l_value_char = fleet_info-consum_s.
  ELSE.
*-> if there is no short term, we should have a long term
    l_value_char = fleet_info-consum_l.
  ENDIF.
*-> if we don't have an vallue -> no conversion
  IF NOT l_value_char IS INITIAL.
*-> det. average in SI unit
    CALL FUNCTION 'CHAR_FLTP_CONVERSION_TO_SI'
      EXPORTING
        char_value      = l_value_char
        char_unit       = fleet_info-consum_u
      IMPORTING
        fltp_value_si   = l_value_num
      EXCEPTIONS
        no_unit_given   = 1
        string_not_fltp = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      p_subrc = 1.
      EXIT.
    ENDIF.
  ENDIF.
*-> if we don't have an average -> don't perform a check
  IF NOT l_value_num IS INITIAL.
*-> calculate destination
    IF t370clc-invert IS INITIAL.
      l_value_num = p_consp_cdiff / l_value_num.
    ELSE.
      l_value_num = p_consp_cdiff * l_value_num.
    ENDIF.
*-> calc. lower
    IF NOT t370con_tol-neg_prec IS INITIAL.
      l_value_min = l_value_num -
        ( l_value_num * t370con_tol-neg_prec / 100 ).
      l_value_min = p_count_cntrr -
                    p_count_cdiff + l_value_min.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          char_unit     = p_count_unit
          decimals      = p_count_decim
          fltp_value_si = l_value_min
        IMPORTING
          char_value    = p_value_min_char
        EXCEPTIONS
          no_unit_given = 1
          OTHERS        = 2.
      CONDENSE p_value_min_char.
*-> adapt FLTP value to rounding
      CALL FUNCTION 'CHAR_FLTP_CONVERSION_TO_SI'
        EXPORTING
          char_value      = p_value_min_char
          char_unit       = p_count_unit
        IMPORTING
          fltp_value_si   = l_value_min
        EXCEPTIONS
          no_unit_given   = 1
          string_not_fltp = 2
          OTHERS          = 3.
    ENDIF.
*-> calc upper
    IF NOT t370con_tol-pos_prec IS INITIAL.
      l_value_max = l_value_num +
        ( l_value_num * t370con_tol-pos_prec / 100 ).
      l_value_max = p_count_cntrr -
                    p_count_cdiff + l_value_max.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          char_unit     = p_count_unit
          decimals      = p_count_decim
          fltp_value_si = l_value_max
        IMPORTING
          char_value    = p_value_max_char
        EXCEPTIONS
          no_unit_given = 1
          OTHERS        = 2.
      CONDENSE p_value_max_char.
*-> adapt FLTP value to rounding
      CALL FUNCTION 'CHAR_FLTP_CONVERSION_TO_SI'
        EXPORTING
          char_value      = p_value_max_char
          char_unit       = p_count_unit
        IMPORTING
          fltp_value_si   = l_value_max
        EXCEPTIONS
          no_unit_given   = 1
          string_not_fltp = 2
          OTHERS          = 3.
    ENDIF.
*-> check and raise
    IF NOT l_value_min IS INITIAL AND
       NOT l_value_max IS INITIAL.
*-> range check
      IF p_count_cntrr < l_value_min OR
         p_count_cntrr > l_value_max.
        p_subrc = 2.                                        "range
        EXIT.
      ENDIF.
    ELSEIF NOT l_value_min IS INITIAL.
*-> check lower
      IF p_count_cntrr < l_value_min.
        p_subrc = 3.                                        "to low
        EXIT.
      ENDIF.
    ELSEIF NOT l_value_max IS INITIAL.
*-> check upper
      IF p_count_cntrr > l_value_max.
        p_subrc = 4.                                        "to high
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " CHECK_T370CON_TOL

*&---------------------------------------------------------------------*
*&      Form  BAPIRET2_TO_SYST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_BAPIRET  text
*----------------------------------------------------------------------*
FORM bapiret2_to_syst USING p_bapiret2 TYPE bapiret2.

  sy-msgty = p_bapiret2-type.
  sy-msgid = p_bapiret2-id.
  sy-msgno = p_bapiret2-number.
  sy-msgv1 = p_bapiret2-message_v1.
  sy-msgv2 = p_bapiret2-message_v2.
  sy-msgv3 = p_bapiret2-message_v3.
  sy-msgv4 = p_bapiret2-message_v4.

ENDFORM.                               " BAPIRET2_TO_SYST
