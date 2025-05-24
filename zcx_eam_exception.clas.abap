CLASS zcx_eam_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    METHODS constructor
      IMPORTING textid        LIKE if_t100_message=>t100key OPTIONAL
                !previous     LIKE previous                 OPTIONAL
                eam_fuel_type TYPE string                   OPTIONAL
                eam_staff     TYPE string                   OPTIONAL
                eam_station   TYPE string                   OPTIONAL
                eam_fleet     TYPE string                   OPTIONAL
                eam_auth_txt  TYPE string                   OPTIONAL
                eam_error     TYPE string                   OPTIONAL
                eam_userid    TYPE usnam                    OPTIONAL
                imrg_idate    TYPE string                   OPTIONAL
                imrg_value    TYPE string                   OPTIONAL
                imrg_hours    TYPE string                   OPTIONAL
                imrg_days     TYPE string                   OPTIONAL
                imrg_meins    TYPE meins                    OPTIONAL
                imrg_meins_s  TYPE string                   OPTIONAL.

    DATA eam_fuel_type TYPE string.
    DATA eam_staff     TYPE string.
    DATA eam_station   TYPE string.
    DATA eam_fleet     TYPE string.
    DATA eam_auth_txt  TYPE string.
    DATA eam_error     TYPE string.
    DATA eam_userid    TYPE usnam.
    DATA imrg_idate    TYPE string.
    DATA imrg_value    TYPE string.
    DATA imrg_hours    TYPE string.
    DATA imrg_days     TYPE string.
    DATA imrg_meins    TYPE meins.
    DATA imrg_meins_s  TYPE string.

    CONSTANTS:
      BEGIN OF eam_fuelTypeNotAllowedForFleet,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '001',
        attr1 TYPE scx_attrname VALUE 'EAM_FUEL_TYPE',
        attr2 TYPE scx_attrname VALUE 'EAM_FLEET',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_fuelTypeNotAllowedForFleet.
    CONSTANTS:
      BEGIN OF eam_staffid_is_mandatory,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '002',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_staffid_is_mandatory.
    CONSTANTS:
      BEGIN OF eam_shorttext_is_mandatory,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '003',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_shorttext_is_mandatory.
    CONSTANTS:
      BEGIN OF eam_StaffNotAssignedToStation,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '004',
        attr1 TYPE scx_attrname VALUE 'EAM_STAFF',
        attr2 TYPE scx_attrname VALUE 'EAM_STATION',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_StaffNotAssignedToStation.
    CONSTANTS:
      BEGIN OF eam_FuelTypeMismatchTank_Fleet,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '005',
        attr1 TYPE scx_attrname VALUE 'EAM_FUEL_TYPE',
        attr2 TYPE scx_attrname VALUE 'EAM_FLEET',
        attr3 TYPE scx_attrname VALUE 'EAM_STATION',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_FuelTypeMismatchTank_Fleet.
    CONSTANTS:
      BEGIN OF eam_StaffNotAssignedToAStation,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '006',
        attr1 TYPE scx_attrname VALUE 'EAM_STAFF',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_StaffNotAssignedToAStation.
    CONSTANTS:
      BEGIN OF eam_StaffAssignedToStation,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '007',
        attr1 TYPE scx_attrname VALUE 'EAM_STAFF',
        attr2 TYPE scx_attrname VALUE 'EAM_STATION',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_StaffAssignedToStation.
    CONSTANTS:
      BEGIN OF eam_StaffAssignedTempStation,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '008',
        attr1 TYPE scx_attrname VALUE 'EAM_STAFF',
        attr2 TYPE scx_attrname VALUE 'EAM_STATION',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_StaffAssignedTempStation.
    CONSTANTS:
      BEGIN OF eam_StaffDoesNotExist,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '009',
        attr1 TYPE scx_attrname VALUE 'EAM_STAFF',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_StaffDoesNotExist.
    CONSTANTS:
      BEGIN OF eam_FilePathTooLong,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '010',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_FilePathTooLong.
    CONSTANTS:
      BEGIN OF eam_UnsupportedFileFormat,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '011',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_UnsupportedFileFormat.
    CONSTANTS:
      BEGIN OF eam_ErrorUploadingAttachment,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '012',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_ErrorUploadingAttachment.
    CONSTANTS:
      BEGIN OF eam_MissingAuthorization,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '013',
        attr1 TYPE scx_attrname VALUE 'EAM_AUTH_TXT',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_MissingAuthorization.
    CONSTANTS:
      BEGIN OF eam_IFCUProcessDoesNotExist,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '014',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_IFCUProcessDoesNotExist.
    CONSTANTS:
      BEGIN OF eam_StaffAssignment_Error,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '015',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_StaffAssignment_Error.
    CONSTANTS:
      BEGIN OF eam_TempStaffAssignment_Error,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '016',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_TempStaffAssignment_Error.
    CONSTANTS:
      BEGIN OF eam_PostingFuelConsump_Error,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '017',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_PostingFuelConsump_Error.
    CONSTANTS:
      BEGIN OF eam_Equipment_Is_Mandatory,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '018',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_Equipment_Is_Mandatory.
    CONSTANTS:
      BEGIN OF eam_Station_Is_Mandatory,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '019',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_Station_Is_Mandatory.
    CONSTANTS:
      BEGIN OF eam_Equipment_DoesNotExist,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '020',
        attr1 TYPE scx_attrname VALUE 'EAM_FLEET',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_Equipment_DoesNotExist.
    CONSTANTS:
      BEGIN OF eam_Station_DoesNotExist,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '021',
        attr1 TYPE scx_attrname VALUE 'EAM_STATION',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_Station_DoesNotExist.
    CONSTANTS:
      BEGIN OF eam_Staff_AlreadyAssigned,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '022',
        attr1 TYPE scx_attrname VALUE 'EAM_STAFF',
        attr2 TYPE scx_attrname VALUE 'EAM_STATION',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_Staff_AlreadyAssigned.
    CONSTANTS:
      BEGIN OF eam_StartDate_Is_Mandatory,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '023',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_StartDate_Is_Mandatory.
    CONSTANTS:
      BEGIN OF eam_EndDate_Is_Mandatory,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '024',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_EndDate_Is_Mandatory.
    CONSTANTS:
      BEGIN OF eam_StartDate_Later,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '025',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_StartDate_Later.
    CONSTANTS:
      BEGIN OF eam_StartDate_Earlier,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '026',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_StartDate_Earlier.
    CONSTANTS:
      BEGIN OF eam_User_Cancelled,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '027',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_User_Cancelled.
    CONSTANTS:
      BEGIN OF eam_No_Record_Selected,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '028',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_No_Record_Selected.
    CONSTANTS:
      BEGIN OF eam_AccessReason_Is_Mandatory,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '029',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_AccessReason_Is_Mandatory.
    CONSTANTS:
      BEGIN OF eam_alv_error,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '030',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_alv_error.
    CONSTANTS:
      BEGIN OF eam_stationAss_Withdrawn,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '031',
        attr1 TYPE scx_attrname VALUE 'EAM_STATION',
        attr2 TYPE scx_attrname VALUE 'EAM_STAFF',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_stationAss_Withdrawn.
    CONSTANTS:
      BEGIN OF eam_noDoc_Title,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '032',
        attr1 TYPE scx_attrname VALUE 'EAM_STATION',
        attr2 TYPE scx_attrname VALUE 'EAM_STAFF',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_noDoc_Title.
    CONSTANTS:
      BEGIN OF eam_doc_upload_success,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '033',
        attr1 TYPE scx_attrname VALUE 'EAM_STATION',
        attr2 TYPE scx_attrname VALUE 'EAM_STAFF',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_doc_upload_success.
    CONSTANTS:
      BEGIN OF eam_gen_error,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '034',
        attr1 TYPE scx_attrname VALUE 'EAM_ERROR',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_gen_error.
    CONSTANTS:
      BEGIN OF eam_User_staff_not_assigned,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '035',
        attr1 TYPE scx_attrname VALUE 'EAM_USERID',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_user_staff_not_assigned.
    CONSTANTS:
      BEGIN OF eam_report_generation,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '036',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_report_generation.
    CONSTANTS:
      BEGIN OF eam_null_records,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '037',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_null_records.
    CONSTANTS:
      BEGIN OF eam_null_document,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '038',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_null_document.
    CONSTANTS:
      BEGIN OF eam_auth_fail_doc,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '039',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_auth_fail_doc.
    CONSTANTS:
      BEGIN OF eam_doc_error,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '040',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_doc_error.
    CONSTANTS:
      BEGIN OF eam_other_doc_error,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '041',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_other_doc_error.
    CONSTANTS:
      BEGIN OF eam_doc_success,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '042',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF eam_doc_success.
    CONSTANTS:
      BEGIN OF imrg_rec_date_error,
        msgid TYPE symsgid      VALUE 'ZPM_IMRG',
        msgno TYPE symsgno      VALUE '001',
        attr1 TYPE scx_attrname VALUE 'IMRG_IDATE',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF imrg_rec_date_error.
    CONSTANTS:
      BEGIN OF imrg_rec_value_error,
        msgid TYPE symsgid      VALUE 'ZPM_IMRG',
        msgno TYPE symsgno      VALUE '002',
        attr1 TYPE scx_attrname VALUE 'IMRG_VALUE',
        attr2 TYPE scx_attrname VALUE 'IMRG_MEINS_S',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF imrg_rec_value_error.
    CONSTANTS:
      BEGIN OF imrg_rec_value_high,
        msgid TYPE symsgid      VALUE 'ZPM_IMRG',
        msgno TYPE symsgno      VALUE '003',
        attr1 TYPE scx_attrname VALUE 'IMRG_HOURS',
        attr2 TYPE scx_attrname VALUE 'IMRG_MEINS_S',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF imrg_rec_value_high.
    CONSTANTS:
      BEGIN OF imrg_rec_value_low,
        msgid TYPE symsgid      VALUE 'ZPM_IMRG',
        msgno TYPE symsgno      VALUE '004',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF imrg_rec_value_low.
    CONSTANTS:
      BEGIN OF imrg_rec_error_005,
        msgid TYPE symsgid      VALUE 'ZEAM_MSG',
        msgno TYPE symsgno      VALUE '045',
        attr1 TYPE scx_attrname VALUE 'IMRG_VALUE',
        attr2 TYPE scx_attrname VALUE 'IMRG_DAYS',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF imrg_rec_error_005.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_eam_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->eam_fuel_type = eam_fuel_type.
    me->eam_staff     = eam_staff.
    me->eam_station   = eam_station.
    me->eam_auth_txt  = eam_auth_txt.
    me->eam_fleet     = eam_fleet.
    me->eam_error     = eam_error.
    me->eam_userid    = eam_userid.
    me->imrg_idate    = imrg_idate.
    me->imrg_value    = imrg_value.
    me->imrg_hours    = imrg_hours.
    me->imrg_days     = imrg_days.
    me->imrg_meins    = |{ imrg_meins ALPHA = OUT }|.

    me->imrg_meins_s  = imrg_meins_s.

    IF xsdbool( me->imrg_meins_s IS INITIAL AND imrg_meins IS NOT INITIAL )
       = abap_true.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING input      = imrg_meins
                  language   = sy-langu
        IMPORTING short_text = me->imrg_meins_s.
    ENDIF.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
