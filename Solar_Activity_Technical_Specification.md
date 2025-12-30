# Technical Specification: Solar Activity - Sales Order Handling Unit Assignment/Unassignment and SFG to FG Conversion

## Overview
This document provides the technical implementation details for the Solar Activity Function Module that handles Sales Order assignment/unassignment to Handling Units and SFG to FG conversion operations in the EWM system.

## System Requirements
- **Target System**: SAP ECC 6.0 / NetWeaver 7.31
- **Syntax Level**: abap_731
- **Enforcement**: Strict compliance with ABAP Code Rules

## Function Module Structure

### Function Module Information
- **Function Module Name**: `Z_SOLAR_ACTIVITY` or `Y_SOLAR_ACTIVITY`
- **Function Group**: `ZFG_SOLAR_ACTIVITY` or `YFG_SOLAR_ACTIVITY`
- **RFC Enabled**: Yes (for remote calls if required)
- **Update Task**: No (standard call)
- **Remote-Enabled Module**: Yes

### Function Group Includes Structure

```
ZFG_SOLAR_ACTIVITYTOP    " Global declarations (TOP) - MANDATORY
ZFG_SOLAR_ACTIVITYUXX    " Function Module code (UXX) - MANDATORY
ZFG_SOLAR_ACTIVITYF01    " Subroutines (F01) - Optional
```

## Function Module Interface

### Function Module Definition

```abap
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
```

## Type Definitions (in TOP include)

### Global Types

```abap
" Activity Code Constants
CONSTANTS: gc_activity_assign TYPE char2 VALUE 'P1',
           gc_activity_unassign TYPE char2 VALUE 'P2',
           gc_activity_sfg_fg TYPE char2 VALUE 'A2'.

" Status Constants
CONSTANTS: gc_status_pending TYPE char20 VALUE 'PENDING',
           gc_status_running TYPE char20 VALUE 'RUNNING',
           gc_status_completed TYPE char20 VALUE 'COMPLETED',
           gc_status_failed TYPE char20 VALUE 'FAILED',
           gc_status_partial TYPE char20 VALUE 'PARTIAL_SUCCESS'.

" Batch Size Constant
CONSTANTS: gc_batch_size TYPE i VALUE 10,
           gc_max_hus TYPE i VALUE 1000.

" Handling Unit Structure
TYPES: BEGIN OF gty_handling_unit,
         hu_number TYPE char20,
         pallet_number TYPE char20,
         carton_number TYPE char20,
       END OF gty_handling_unit.

" Handling Units Table Type (MANDATORY - specific type, not generic)
TYPES: gty_handling_units TYPE TABLE OF gty_handling_unit.

" Batch Details Structure
TYPES: BEGIN OF gty_batch_detail,
         batch_number TYPE i,
         status TYPE char20,
         records_count TYPE i,
         error_message TYPE string,
       END OF gty_batch_detail.

" Batch Details Table Type
TYPES: gty_batch_details TYPE TABLE OF gty_batch_detail.

" Error Log Structure
TYPES: BEGIN OF gty_error_log,
         transaction_id TYPE numc10,
         handling_unit TYPE char20,
         error_code TYPE char10,
         error_message TYPE string,
       END OF gty_error_log.

" Error Log Table Type
TYPES: gty_error_log TYPE TABLE OF gty_error_log.

" Transaction Header Structure
TYPES: BEGIN OF gty_transaction_header,
         transaction_id TYPE numc10,
         num_pallets TYPE int4,
         created_by TYPE uname,
         created_date TYPE datum,
         created_time TYPE uzeit,
         activity TYPE char2,
         status TYPE char20,
       END OF gty_transaction_header.

" Transaction Item Structure
TYPES: BEGIN OF gty_transaction_item,
         transaction_id TYPE numc10,
         pallet_number TYPE char20,
         carton_number TYPE char20,
       END OF gty_transaction_item.

" Transaction Items Table Type
TYPES: gty_transaction_items TYPE TABLE OF gty_transaction_item.
```

## Function Module Implementation

### Main Function Module Code

```abap
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

  " Local variable declarations
  DATA: lv_subrc TYPE sy-subrc,
        lv_count TYPE i,
        lv_duplicate_count TYPE i,
        lv_transaction_id TYPE numc10,
        lv_batch_number TYPE i,
        lv_total_batches TYPE i,
        lv_success_count TYPE i,
        lv_fail_count TYPE i,
        lv_index TYPE i,
        lv_batch_size TYPE i,
        lv_num_pallets TYPE int4,
        lv_num_cartons TYPE int4,
        lw_header TYPE zpaltrfhdr,
        lw_item TYPE zpaltrfitm,
        lw_batch_detail TYPE gty_batch_detail,
        lw_error_log TYPE gty_error_log,
        lw_message TYPE bapiret2,
        lt_batch_hus TYPE gty_handling_units,
        lt_unique_hus TYPE gty_handling_units,
        lw_hu TYPE gty_handling_unit,
        lt_aqua TYPE TABLE OF /scwm/aqua,
        lw_aqua TYPE /scwm/aqua,
        lt_vbak TYPE TABLE OF vbak,
        lw_vbak TYPE vbak,
        lt_vbap TYPE TABLE OF vbap,
        lt_return TYPE TABLE OF bapiret2,
        lw_return TYPE bapiret2,
        lt_posting_data TYPE TABLE OF zscm_bin_intl_data,
        lw_posting_data TYPE zscm_bin_intl_data.

  FIELD-SYMBOLS: <lfs_hu> TYPE gty_handling_unit.

  " Initialize output parameters
  CLEAR: ev_transaction_id,
         ev_total_hus,
         ev_success_count,
         ev_fail_count,
         ev_status,
         ev_num_pallets,
         ev_num_cartons,
         et_batch_details,
         et_error_log,
         et_messages.

  " Step 1: Authorization Check
  AUTHORITY-CHECK OBJECT 'S_RFC'
    ID 'RFC_TYPE' FIELD 'FUNC'
    ID 'RFC_NAME' FIELD 'Z_SOLAR_ACTIVITY'
    ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    lw_message-type = 'E'.
    lw_message-id = 'ZMSG'.
    lw_message-number = '001'.
    lw_message-message = 'Not authorized to execute function module'.
    APPEND lw_message TO et_messages.
    RAISE no_authority.
  ENDIF.

  " Step 2: Validate Activity Code
  IF iv_activity_code <> gc_activity_assign AND
     iv_activity_code <> gc_activity_unassign AND
     iv_activity_code <> gc_activity_sfg_fg.
    lw_message-type = 'E'.
    lw_message-id = 'ZMSG'.
    lw_message-number = '002'.
    lw_message-message_v1 = iv_activity_code.
    lw_message-message = 'Invalid activity code'.
    APPEND lw_message TO et_messages.
    RAISE invalid_activity_code.
  ENDIF.

  " Step 3: Validate Handling Units Input
  DESCRIBE TABLE it_handling_units LINES lv_count.
  ev_total_hus = lv_count.

  IF lv_count = 0.
    lw_message-type = 'E'.
    lw_message-id = 'ZMSG'.
    lw_message-number = '003'.
    lw_message-message = 'No handling units provided'.
    APPEND lw_message TO et_messages.
    RAISE invalid_handling_unit.
  ENDIF.

  IF lv_count > gc_max_hus.
    lw_message-type = 'E'.
    lw_message-id = 'ZMSG'.
    lw_message-number = '004'.
    lw_message-message_v1 = gc_max_hus.
    lw_message-message = 'Maximum handling unit limit exceeded'.
    APPEND lw_message TO et_messages.
    RAISE max_limit_exceeded.
  ENDIF.

  " Check for duplicates
  lt_unique_hus = it_handling_units.
  SORT lt_unique_hus BY hu_number.
  DELETE ADJACENT DUPLICATES FROM lt_unique_hus COMPARING hu_number.
  DESCRIBE TABLE lt_unique_hus LINES lv_duplicate_count.

  IF lv_count <> lv_duplicate_count.
    lw_message-type = 'E'.
    lw_message-id = 'ZMSG'.
    lw_message-number = '005'.
    lw_message-message = 'Duplicate handling units found'.
    APPEND lw_message TO et_messages.
    RAISE duplicate_handling_units.
  ENDIF.

  " Step 4: Validate Handling Units against /SCWM/AQUA
  IF it_handling_units IS NOT INITIAL.
    LOOP AT it_handling_units INTO lw_hu.
      SELECT SINGLE guid huident
        FROM /scwm/aqua
        INTO lw_aqua
        WHERE huident = lw_hu-hu_number.
      IF sy-subrc <> 0.
        lw_message-type = 'E'.
        lw_message-id = 'ZMSG'.
        lw_message-number = '006'.
        lw_message-message_v1 = lw_hu-hu_number.
        lw_message-message = 'Handling unit does not exist'.
        APPEND lw_message TO et_messages.
        lw_error_log-handling_unit = lw_hu-hu_number.
        lw_error_log-error_code = 'VAL-003'.
        lw_error_log-error_message = 'Handling unit does not exist in EWM'.
        APPEND lw_error_log TO et_error_log.
      ENDIF.
    ENDLOOP.

    IF lines( et_error_log ) > 0.
      RAISE invalid_handling_unit.
    ENDIF.
  ENDIF.

  " Step 5: Activity-specific validations
  IF iv_activity_code = gc_activity_assign OR
     iv_activity_code = gc_activity_unassign.
    " Validate Sales Order
    IF iv_sales_order IS INITIAL.
      lw_message-type = 'E'.
      lw_message-id = 'ZMSG'.
      lw_message-number = '007'.
      lw_message-message = 'Sales order is required for this activity'.
      APPEND lw_message TO et_messages.
      RAISE invalid_sales_order.
    ENDIF.

    SELECT SINGLE vbeln erdat auart
      FROM vbak
      INTO lw_vbak
      WHERE vbeln = iv_sales_order.

    IF sy-subrc <> 0.
      lw_message-type = 'E'.
      lw_message-id = 'ZMSG'.
      lw_message-number = '008'.
      lw_message-message_v1 = iv_sales_order.
      lw_message-message = 'Sales order does not exist'.
      APPEND lw_message TO et_messages.
      RAISE invalid_sales_order.
    ENDIF.

    " Validate Storage Location
    IF iv_storage_location IS INITIAL.
      lw_message-type = 'E'.
      lw_message-id = 'ZMSG'.
      lw_message-number = '009'.
      lw_message-message = 'Storage location is required for this activity'.
      APPEND lw_message TO et_messages.
      RAISE invalid_storage_location.
    ENDIF.

    " Call validation function module
    CALL FUNCTION 'ZSCM_SO_SLOC_VALIDATION'
      EXPORTING
        iv_sales_order = iv_sales_order
        iv_storage_location = iv_storage_location
      IMPORTING
        et_return = lt_return
      EXCEPTIONS
        error = 1
        OTHERS = 2.

    lv_subrc = sy-subrc.

    IF lv_subrc <> 0 OR lines( lt_return ) > 0.
      LOOP AT lt_return INTO lw_return WHERE type = 'E' OR type = 'A'.
        APPEND lw_return TO et_messages.
      ENDLOOP.
      IF lines( et_messages ) > 0.
        RAISE validation_error.
      ENDIF.
    ENDIF.

  ELSEIF iv_activity_code = gc_activity_sfg_fg.
    " Validate Warehouse
    IF iv_warehouse IS INITIAL.
      lw_message-type = 'E'.
      lw_message-id = 'ZMSG'.
      lw_message-number = '010'.
      lw_message-message = 'Warehouse is required for SFG to FG conversion'.
      APPEND lw_message TO et_messages.
      RAISE invalid_warehouse.
    ENDIF.

    " Validate Plant
    IF iv_plant IS INITIAL.
      lw_message-type = 'E'.
      lw_message-id = 'ZMSG'.
      lw_message-number = '011'.
      lw_message-message = 'Plant is required for SFG to FG conversion'.
      APPEND lw_message TO et_messages.
      RAISE invalid_plant.
    ENDIF.
  ENDIF.

  " Step 6: Create Transaction Header
  SELECT MAX( transaction_id )
    FROM zpaltrfhdr
    INTO lv_transaction_id.

  IF sy-subrc = 0 AND lv_transaction_id IS NOT INITIAL.
    lv_transaction_id = lv_transaction_id + 1.
  ELSE.
    lv_transaction_id = '0000000001'.
  ENDIF.

  " Count pallets and cartons
  LOOP AT it_handling_units INTO lw_hu.
    IF lw_hu-pallet_number IS NOT INITIAL.
      lv_num_pallets = lv_num_pallets + 1.
    ENDIF.
    IF lw_hu-carton_number IS NOT INITIAL.
      lv_num_cartons = lv_num_cartons + 1.
    ENDIF.
  ENDLOOP.

  " Prepare header record
  lw_header-transaction_id = lv_transaction_id.
  lw_header-num_pallets = lv_num_pallets.
  lw_header-created_by = sy-uname.
  lw_header-created_date = sy-datum.
  lw_header-created_time = sy-uzeit.
  lw_header-activity = iv_activity_code.
  lw_header-status = gc_status_pending.

  " Insert header
  INSERT zpaltrfhdr FROM lw_header.

  IF sy-subrc <> 0.
    lw_message-type = 'E'.
    lw_message-id = 'ZMSG'.
    lw_message-number = '012'.
    lw_message-message = 'Failed to create transaction header'.
    APPEND lw_message TO et_messages.
    RAISE database_error.
  ENDIF.

  " Step 7: Create Transaction Items
  LOOP AT it_handling_units INTO lw_hu.
    lw_item-transaction_id = lv_transaction_id.
    lw_item-pallet_number = lw_hu-pallet_number.
    lw_item-carton_number = lw_hu-carton_number.

    INSERT zpaltrfitm FROM lw_item.
    IF sy-subrc <> 0.
      lw_message-type = 'E'.
      lw_message-id = 'ZMSG'.
      lw_message-number = '013'.
      lw_message-message_v1 = lw_hu-pallet_number.
      lw_message-message = 'Failed to insert item'.
      APPEND lw_message TO et_messages.
      RAISE database_error.
    ENDIF.
  ENDLOOP.

  " Step 8: Batch Processing
  lv_batch_size = gc_batch_size.
  lv_total_batches = ( lv_count + lv_batch_size - 1 ) / lv_batch_size.
  lv_batch_number = 1.
  lv_index = 1.

  " Update status to RUNNING
  lw_header-status = gc_status_running.
  UPDATE zpaltrfhdr FROM lw_header.

  WHILE lv_index <= lv_count.
    CLEAR lt_batch_hus.

    " Collect next batch
    DO lv_batch_size TIMES.
      READ TABLE it_handling_units INTO lw_hu INDEX lv_index.
      IF sy-subrc = 0.
        APPEND lw_hu TO lt_batch_hus.
        lv_index = lv_index + 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    " Prepare batch detail
    CLEAR lw_batch_detail.
    lw_batch_detail-batch_number = lv_batch_number.
    lw_batch_detail-records_count = lines( lt_batch_hus ).
    lw_batch_detail-status = gc_status_running.

    " Prepare posting data
    CLEAR lt_posting_data.
    LOOP AT lt_batch_hus INTO lw_hu.
      CLEAR lw_posting_data.
      lw_posting_data-hu_number = lw_hu-hu_number.
      lw_posting_data-pallet_number = lw_hu-pallet_number.
      lw_posting_data-carton_number = lw_hu-carton_number.
      APPEND lw_posting_data TO lt_posting_data.
    ENDLOOP.

    " Call Function Module to get posting details
    CLEAR lt_return.
    CALL FUNCTION 'ZSCM_BIN_INTL_POST_GET_DET'
      EXPORTING
        it_handling_units = lt_posting_data
        iv_action_code = iv_activity_code
      IMPORTING
        et_return = lt_return
      EXCEPTIONS
        error = 1
        OTHERS = 2.

    lv_subrc = sy-subrc.

    IF lv_subrc <> 0.
      lw_batch_detail-status = gc_status_failed.
      lw_batch_detail-error_message = 'Failed to get posting details'.
      APPEND lw_batch_detail TO et_batch_details.
      lv_fail_count = lv_fail_count + lines( lt_batch_hus ).
      
      LOOP AT lt_batch_hus INTO lw_hu.
        CLEAR lw_error_log.
        lw_error_log-transaction_id = lv_transaction_id.
        lw_error_log-handling_unit = lw_hu-hu_number.
        lw_error_log-error_code = 'FM-001'.
        lw_error_log-error_message = 'Function Module ZSCM_BIN_INTL_POST_GET_DET failed'.
        APPEND lw_error_log TO et_error_log.
      ENDLOOP.

      LOOP AT lt_return INTO lw_return WHERE type = 'E' OR type = 'A'.
        APPEND lw_return TO et_messages.
      ENDLOOP.

    ELSE.
      " Call Function Module to execute posting
      CLEAR lt_return.
      CALL FUNCTION 'ZSCM_BIN_INTL_POSTING'
        EXPORTING
          it_posting_data = lt_posting_data
          iv_action_code = iv_activity_code
        IMPORTING
          et_return = lt_return
        EXCEPTIONS
          error = 1
          OTHERS = 2.

      lv_subrc = sy-subrc.

      IF lv_subrc <> 0.
        lw_batch_detail-status = gc_status_failed.
        lw_batch_detail-error_message = 'Posting failed'.
        APPEND lw_batch_detail TO et_batch_details.
        lv_fail_count = lv_fail_count + lines( lt_batch_hus ).

        LOOP AT lt_batch_hus INTO lw_hu.
          CLEAR lw_error_log.
          lw_error_log-transaction_id = lv_transaction_id.
          lw_error_log-handling_unit = lw_hu-hu_number.
          lw_error_log-error_code = 'FM-002'.
          lw_error_log-error_message = 'Function Module ZSCM_BIN_INTL_POSTING failed'.
          APPEND lw_error_log TO et_error_log.
        ENDLOOP.

        LOOP AT lt_return INTO lw_return WHERE type = 'E' OR type = 'A'.
          APPEND lw_return TO et_messages.
        ENDLOOP.

      ELSE.
        " Check for errors in return table
        LOOP AT lt_return INTO lw_return WHERE type = 'E' OR type = 'A'.
          lw_batch_detail-status = gc_status_failed.
          lw_batch_detail-error_message = lw_return-message.
          lv_fail_count = lv_fail_count + 1.
          APPEND lw_return TO et_messages.
        ENDLOOP.

        IF lw_batch_detail-status IS INITIAL.
          lw_batch_detail-status = gc_status_completed.
          lv_success_count = lv_success_count + lines( lt_batch_hus ).
        ENDIF.

        APPEND lw_batch_detail TO et_batch_details.
      ENDIF.
    ENDIF.

    lv_batch_number = lv_batch_number + 1.
  ENDWHILE.

  " Step 9: Update Final Status
  IF lv_fail_count = 0.
    ev_status = gc_status_completed.
    lw_header-status = gc_status_completed.
  ELSEIF lv_success_count > 0.
    ev_status = gc_status_partial.
    lw_header-status = gc_status_partial.
  ELSE.
    ev_status = gc_status_failed.
    lw_header-status = gc_status_failed.
  ENDIF.

  UPDATE zpaltrfhdr FROM lw_header.

  " Set output parameters
  ev_transaction_id = lv_transaction_id.
  ev_success_count = lv_success_count.
  ev_fail_count = lv_fail_count.
  ev_num_pallets = lv_num_pallets.
  ev_num_cartons = lv_num_cartons.

  " Add success message if no errors
  IF lines( et_messages ) = 0 AND ev_status = gc_status_completed.
    CLEAR lw_message.
    lw_message-type = 'S'.
    lw_message-id = 'ZMSG'.
    lw_message-number = '014'.
    lw_message-message = 'Transaction processed successfully'.
    APPEND lw_message TO et_messages.
  ENDIF.

ENDFUNCTION.
```

## Subroutines (Optional - in F01 include)

If complex logic needs to be extracted into subroutines:

```abap
" Validate handling unit against /SCWM/AQUA
FORM validate_handling_unit
  USING
    iv_hu_number TYPE char20
  CHANGING
    cv_valid TYPE abap_bool
    cv_error_message TYPE string.

  DATA: lw_aqua TYPE /scwm/aqua.

  cv_valid = abap_false.

  SELECT SINGLE guid huident
    FROM /scwm/aqua
    INTO lw_aqua
    WHERE huident = iv_hu_number.

  IF sy-subrc = 0.
    cv_valid = abap_true.
  ELSE.
    cv_error_message = 'Handling unit does not exist'.
  ENDIF.

ENDFORM.
```

## Performance Considerations

1. **Database Access**:
   - Use `FOR ALL ENTRIES` instead of `SELECT` in loops
   - Always check `IS NOT INITIAL` before `FOR ALL ENTRIES`
   - Use specific field lists in SELECT statements (no SELECT *)

2. **Batch Processing**:
   - Process in batches of 10 handling units
   - Update status after each batch
   - Commit work after each successful batch (if using update task)

3. **Memory Management**:
   - Clear internal tables after use
   - Use field symbols for table modifications
   - Avoid nested loops

4. **Error Handling**:
   - Catch exceptions at appropriate levels
   - Log errors without stopping batch processing
   - Provide detailed error messages in return table

## Security Implementation

1. **Authorization Checks**:
   - RFC authorization (S_RFC)
   - Goods movement authorization (M_MATE_WRK) - if required
   - Custom authorization object (if required)

2. **Input Validation**:
   - Validate all input parameters
   - Check for SQL injection risks
   - Validate handling unit format

3. **Audit Trail**:
   - Log all transactions with user ID and timestamp
   - Maintain error logs
   - Track status changes

## Testing Requirements

1. **Unit Tests**:
   - Test each validation independently
   - Mock function module calls
   - Test error scenarios

2. **Integration Tests**:
   - Test with real database
   - Test function module integration
   - Test batch processing

3. **Performance Tests**:
   - Test with maximum 1000 handling units
   - Measure batch processing time
   - Check memory usage

## Code Review Checklist

- [ ] All variables declared upfront (no inline declarations)
- [ ] No SELECT * statements
- [ ] FOR ALL ENTRIES used instead of SELECT in loops
- [ ] Field symbols used for table modifications
- [ ] Exception handling implemented
- [ ] Authorization checks in place
- [ ] No hard-coded values (use constants)
- [ ] Text elements used for messages
- [ ] NetWeaver 7.31 compatible syntax
- [ ] Specific table types used (not generic TABLE)
- [ ] RFC enabled if required
- [ ] Proper exception raising

## Deployment Notes

1. **Transport Request**: Create transport request for all objects
2. **Database Tables**: Ensure ZPALTRFHDR and ZPALTRFITM tables exist
3. **Function Modules**: Ensure ZSCM_SO_SLOC_VALIDATION, ZSCM_BIN_INTL_POST_GET_DET, ZSCM_BIN_INTL_POSTING exist
4. **Authorization**: Create authorization profiles for S_RFC
5. **Message Class**: Create message class ZMSG for error messages
6. **Function Group**: Create function group ZFG_SOLAR_ACTIVITY
7. **Function Module**: Create function module Z_SOLAR_ACTIVITY in SE37
8. **RFC Settings**: Enable RFC if remote calls are required

## Function Module Call Example

```abap
DATA: lv_activity_code TYPE char2 VALUE 'P1',
      lv_sales_order TYPE vbeln VALUE '0000000123',
      lv_storage_location TYPE lgort VALUE '0001',
      lv_transaction_id TYPE numc10,
      lv_total_hus TYPE int4,
      lv_success_count TYPE int4,
      lv_fail_count TYPE int4,
      lv_status TYPE char20,
      lv_num_pallets TYPE int4,
      lv_num_cartons TYPE int4,
      lt_handling_units TYPE gty_handling_units,
      lw_hu TYPE gty_handling_unit,
      lt_batch_details TYPE gty_batch_details,
      lt_error_log TYPE gty_error_log,
      lt_messages TYPE bapiret2_t.

" Prepare handling units
lw_hu-hu_number = 'HU001'.
lw_hu-pallet_number = 'PAL001'.
lw_hu-carton_number = 'CART001'.
APPEND lw_hu TO lt_handling_units.

" Call function module
CALL FUNCTION 'Z_SOLAR_ACTIVITY'
  EXPORTING
    iv_activity_code = lv_activity_code
    iv_sales_order = lv_sales_order
    iv_storage_location = lv_storage_location
  IMPORTING
    ev_transaction_id = lv_transaction_id
    ev_total_hus = lv_total_hus
    ev_success_count = lv_success_count
    ev_fail_count = lv_fail_count
    ev_status = lv_status
    ev_num_pallets = lv_num_pallets
    ev_num_cartons = lv_num_cartons
  TABLES
    it_handling_units = lt_handling_units
    et_batch_details = lt_batch_details
    et_error_log = lt_error_log
    et_messages = lt_messages
  EXCEPTIONS
    invalid_activity_code = 1
    invalid_handling_unit = 2
    invalid_sales_order = 3
    invalid_storage_location = 4
    no_authority = 5
    max_limit_exceeded = 6
    duplicate_handling_units = 7
    function_module_error = 8
    database_error = 9
    validation_error = 10
    OTHERS = 11.

IF sy-subrc = 0.
  " Success - check messages for details
  DATA: lw_message TYPE bapiret2.
  LOOP AT lt_messages INTO lw_message.
    " Process messages
  ENDLOOP.
ELSE.
  " Handle exception
  CASE sy-subrc.
    WHEN 1.
      " Invalid activity code
    WHEN 2.
      " Invalid handling unit
    " ... other cases
  ENDCASE.
ENDIF.
```
