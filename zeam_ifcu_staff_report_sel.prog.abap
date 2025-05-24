*&---------------------------------------------------------------------*
*& Include          ZEAM_IFCU_STAFF_REPORT_SEL
*&---------------------------------------------------------------------*
selection-screen begin of block ba with frame title tta.
    parameters: p_s_read RADIOBUTTON GROUP r1 USER-COMMAND cc DEFAULT 'X',
                p_m_read RADIOBUTTON GROUP r1.
selection-screen end of block ba.
selection-screen begin of block b2 with frame title t2.
 selection-screen begin of line.
        selection-screen comment 01(4) logo6 modif id sc.
        selection-screen comment 05(20) title6 modif id sc .
        parameters: p_equnr type equnr modif id sc.
    selection-screen end of line.
 selection-screen begin of line.
        selection-screen comment 01(4) logo7 modif id sc.
        selection-screen comment 05(20) title7 modif id sc .
        parameters: p_stn type station_t MATCHCODE OBJECT H_STATION modif id sc.
    selection-screen end of line.
 selection-screen begin of line.
        selection-screen comment 01(4) logo8 modif id sc.
        selection-screen comment 05(20) title8 modif id sc .
        parameters: p_pdate type imrc_idate modif id sc.
    selection-screen end of line.
selection-screen end of block b2.
selection-screen begin of block b1 with frame title t1.
 selection-screen begin of line.
        selection-screen comment 01(4) logo1 modif id dc.
        selection-screen comment 05(20) title1 modif id dc .
        select-options: s_eqnr for gs_output-equnr modif id dc.
    selection-screen end of line.
    selection-screen begin of line.
        selection-screen comment 01(4) logo2 MODIF ID dc.
        selection-screen comment 05(20) title2 MODIF ID dc.
        select-options: s_stn for gs_output-station MATCHCODE OBJECT H_STATION MODIF ID dc.
    selection-screen end of line.
    selection-screen begin of line.
        selection-screen comment 01(4) logo3 MODIF ID dc.
        selection-screen comment 05(20) title3 MODIF ID dc.
        select-options: s_pdate for gs_output-posting_date MODIF ID dc.
    selection-screen end of line.
    selection-screen begin of line.
        selection-screen comment 01(4) logo4 MODIF ID dc.
        selection-screen comment 05(20) title4 MODIF ID dc.
        select-options: s_coll for gs_output-collector_staff_id MATCHCODE OBJECT fac_pernr MODIF ID dc.
    selection-screen end of line.
    selection-screen begin of line.
        selection-screen comment 01(4) logo5 MODIF ID dc.
        selection-screen comment 05(20) title5 MODIF ID dc.
        select-options: s_att for gs_output-attendant_staff_id MATCHCODE OBJECT fac_pernr MODIF ID dc.
    selection-screen end of line.
selection-screen end of block b1.
