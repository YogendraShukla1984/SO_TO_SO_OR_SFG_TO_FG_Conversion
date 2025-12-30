*&---------------------------------------------------------------------*
*& Include          ZFG_SOLAR_ACTIVITYTOP
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Global Interface:
*"  This include contains global type definitions and constants
*"  for Function Group ZFG_SOLAR_ACTIVITY
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

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

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*

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

" Handling Unit Number Table (for FOR ALL ENTRIES)
TYPES: gty_hu_numbers TYPE TABLE OF char20.

