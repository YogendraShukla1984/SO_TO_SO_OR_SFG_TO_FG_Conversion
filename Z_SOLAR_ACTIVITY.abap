*&---------------------------------------------------------------------*
*& Function Module: Z_SOLAR_ACTIVITY
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_ACTIVITY_CODE) TYPE  CHAR2
*"     VALUE(IV_SALES_ORDER) TYPE  VBELN OPTIONAL
*"     VALUE(IV_STORAGE_LOCATION) TYPE  LGORT OPTIONAL
*"     VALUE(IV_WAREHOUSE) TYPE  LGNUM OPTIONAL
*"     VALUE(IV_PLANT) TYPE  WERKS_D OPTIONAL
*"  EXPORTING
*"     VALUE(EV_TRANSACTION_ID) TYPE  NUMC10
*"     VALUE(EV_TOTAL_HUS) TYPE  INT4
*"     VALUE(EV_SUCCESS_COUNT) TYPE  INT4
*"     VALUE(EV_FAIL_COUNT) TYPE  INT4
*"     VALUE(EV_STATUS) TYPE  CHAR20
*"     VALUE(EV_NUM_PALLETS) TYPE  INT4
*"     VALUE(EV_NUM_CARTONS) TYPE  INT4
*"  TABLES
*"     IT_HANDLING_UNITS TYPE  GTY_HANDLING_UNITS
*"     ET_BATCH_DETAILS TYPE  GTY_BATCH_DETAILS
*"     ET_ERROR_LOG TYPE  GTY_ERROR_LOG
*"     ET_MESSAGES TYPE  BAPIRET2_T
*"  EXCEPTIONS
*"     INVALID_ACTIVITY_CODE
*"     INVALID_HANDLING_UNIT
*"     INVALID_SALES_ORDER
*"     INVALID_STORAGE_LOCATION
*"     INVALID_WAREHOUSE
*"     INVALID_PLANT
*"     NO_AUTHORITY
*"     MAX_LIMIT_EXCEEDED
*"     DUPLICATE_HANDLING_UNITS
*"     FUNCTION_MODULE_ERROR
*"     DATABASE_ERROR
*"     VALIDATION_ERROR
*"----------------------------------------------------------------------
*"  Function Module: Z_SOLAR_ACTIVITY
*"  Purpose: Assign/Unassign Sales Order to Handling Units and
*"           SFG to FG Conversion
*"  Author: Generated per Technical Specification
*"  Date: Generated
*"----------------------------------------------------------------------

FUNCTION z_solar_activity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_ACTIVITY_CODE) TYPE  CHAR2
*"     VALUE(IV_SALES_ORDER) TYPE  VBELN OPTIONAL
*"     VALUE(IV_STORAGE_LOCATION) TYPE  LGORT OPTIONAL
*"     VALUE(IV_WAREHOUSE) TYPE  LGNUM OPTIONAL
*"     VALUE(IV_PLANT) TYPE  WERKS_D OPTIONAL
*"  EXPORTING
*"     VALUE(EV_TRANSACTION_ID) TYPE  NUMC10
*"     VALUE(EV_TOTAL_HUS) TYPE  INT4
*"     VALUE(EV_SUCCESS_COUNT) TYPE  INT4
*"     VALUE(EV_FAIL_COUNT) TYPE  INT4
*"     VALUE(EV_STATUS) TYPE  CHAR20
*"     VALUE(EV_NUM_PALLETS) TYPE  INT4
*"     VALUE(EV_NUM_CARTONS) TYPE  INT4
*"  TABLES
*"     IT_HANDLING_UNITS TYPE  GTY_HANDLING_UNITS
*"     ET_BATCH_DETAILS TYPE  GTY_BATCH_DETAILS
*"     ET_ERROR_LOG TYPE  GTY_ERROR_LOG
*"     ET_MESSAGES TYPE  BAPIRET2_T
*"  EXCEPTIONS
*"     INVALID_ACTIVITY_CODE
*"     INVALID_HANDLING_UNIT
*"     INVALID_SALES_ORDER
*"     INVALID_STORAGE_LOCATION
*"     INVALID_WAREHOUSE
*"     INVALID_PLANT
*"     NO_AUTHORITY
*"     MAX_LIMIT_EXCEEDED
*"     DUPLICATE_HANDLING_UNITS
*"     FUNCTION_MODULE_ERROR
*"     DATABASE_ERROR
*"     VALIDATION_ERROR
*"----------------------------------------------------------------------

  INCLUDE zfg_solar_activitytop.
  INCLUDE zfg_solar_activityu01.

ENDFUNCTION.

