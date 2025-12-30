# Functional Specification: Solar Activity - Sales Order Handling Unit Assignment/Unassignment and SFG to FG Conversion

## Function Module Information
- **Function Module Name**: `Z_SOLAR_ACTIVITY` or `Y_SOLAR_ACTIVITY`
- **Function Group**: `ZFG_SOLAR_ACTIVITY` or `YFG_SOLAR_ACTIVITY`
- **RFC Enabled**: Yes (for remote calls if required)
- **Update Task**: No (standard call)

## Description
This Function Module enables assignment and unassignment of Sales Orders to/from Handling Units, and transfer of Handling Units to Storage Location (SLOC) in the EWM system. The system processes Handling Units in batches of 10 to optimize performance and manages the complete workflow including validation, posting, and status tracking.

The process involves three main activities:
- **Part 1**: Assign Sales Order to Handling Unit - Links sales orders to handling units and transfers them to storage location
- **Part 2**: Unassign Sales Order from Handling Unit - Removes sales order linkage from handling units
- **Part 3**: SFG to FG Conversion - Converts Semi-Finished Goods to Finished Goods with warehouse and storage location validation

## Inputs

| Field Name | Type | Required | Constraints | Validation Rules |
|------------|------|----------|-------------|------------------|
| Activity Code | CHAR(2) | Yes | Valid values: P1, P2, A2 | P1 = Assign SO, P2 = Unassign SO, A2 = SFG to FG Conversion |
| Handling Units | Table/String List | Yes | Max 1000 entries per transaction | Each HU must exist in `/SCWM/AQUA`, must be valid format, no duplicates allowed |
| Sales Order Number | CHAR(10) | Yes (for P1, P2) | Valid sales order format | Must exist in `VBAK` and `VBAP` tables, must be in valid status |
| Storage Location | CHAR(4) | Yes (for P1, P2) | Valid EWM storage location | Must exist in EWM system, must be accessible by user |
| Warehouse Number | CHAR(3) | Yes (for A2) | Valid warehouse number | Must exist in EWM system |
| Plant | CHAR(4) | Yes (for A2) | Valid plant code | Must exist in system, must be valid for conversion |
| User ID | CHAR(12) | Auto | Current logged-in user | System populated from session |
| Transaction Date | DATS | Auto | Current date | System populated |
| Transaction Time | TIMS | Auto | Current time | System populated |

### Input Validation Rules
- Activity code must be one of: P1 (Assign), P2 (Unassign), A2 (SFG to FG Conversion)
- Handling Units must be between 1 and 1000 entries per transaction
- All Handling Units must be unique within the input list
- Each Handling Unit must exist in `/SCWM/AQUA` table and be in valid status for processing
- Sales Order (for P1/P2) must exist in `VBAK` (header) and `VBAP` (items) tables
- Storage Location must be valid and accessible
- Warehouse, SLOC, and Plant (for A2) must be valid and accessible
- User must have authorization for goods movement and handling unit operations
- Function Module `ZSCM_SO_SLOC_VALIDATION` must be called for validation

## Output

| Field Name | Type | Description |
|------------|------|-------------|
| Transaction ID | NUMC(10) | Auto-generated unique transaction identifier |
| Total Handling Units Processed | Integer | Total number of handling units processed |
| Successful Operations | Integer | Count of successfully processed handling units |
| Failed Operations | Integer | Count of failed operations |
| Batch Details | Table | Details of each batch processed (Batch Number, Status, Records Count, Error Messages) |
| Error Log | Table | Detailed error log with Transaction ID, Handling Unit, Error Code, Error Message |
| Processing Status | String | Overall status: SUCCESS, PARTIAL_SUCCESS, FAILED, RUNNING, COMPLETED |
| Number of Pallets | Integer | Total number of pallets processed |
| Number of Cartons | Integer | Total number of cartons processed |

## Behavior

### Part 1: Assign Sales Order to Handling Unit (Activity Code: P1)

#### Step 1: Input Validation and Transaction Creation
1. User enters Activity Code = P1, Handling Units (max 1000), Sales Order Number, and Storage Location
2. System validates:
   - Activity code is P1
   - Handling Unit count (1-1000)
   - Each Handling Unit format and existence in `/SCWM/AQUA` table
   - Duplicate Handling Units within input
   - Sales Order existence in `VBAK` and `VBAP` tables
   - Storage Location validity
   - User authorization
3. Call Function Module `ZSCM_SO_SLOC_VALIDATION` for Sales Order and Storage Location validation
4. For each valid Handling Unit:
   - Generate auto-incremental Transaction ID
   - Retrieve Pallet Number and Carton Number from Handling Unit data
   - Set Status = 'PENDING'
   - Set Created By = Current User (sy-uname)
   - Set Created On = Current Date (sy-datum)
   - Set Created At = Current Time (sy-uzeit)
   - Set Activity = 'P1'
   - Insert header record into `ZPALTRFHDR` table:
     - Transaction ID
     - Number of Pallets (calculated)
     - Created By / Date / Time
     - Activity = 'P1'
   - Insert item records into `ZPALTRFITM` table:
     - Transaction ID
     - Pallet Number
     - Carton Number

#### Step 2: Batch Processing - Split into Sets of 10
1. System identifies first 10 records with Status = 'PENDING' for the Transaction ID
2. Update Status = 'RUNNING' for the batch
3. Prepare batch data for Function Module calls:
   - Handling Units (10 per batch)
   - Pallet Numbers
   - Carton Numbers
   - Sales Order Number
   - Storage Location
4. Call Function Module `ZSCM_BIN_INTL_POST_GET_DET` to get posting details
5. Call Function Module `ZSCM_BIN_INTL_POSTING` to execute posting
6. Update Status based on Function Module response:
   - If SUCCESS: Status = 'COMPLETED' in `ZPALTRFHDR`
   - If FAILED: Status = 'FAILED', log error details in Error Log
7. Repeat steps 1-6 for next 10 records until all records processed

#### Step 3: Final Status Update
1. System updates overall transaction status:
   - If all batches successful: Status = 'COMPLETED'
   - If any batch failed: Status = 'PARTIAL_SUCCESS' or 'FAILED'
2. Generate summary report with processing results
3. Display results to user

### Part 2: Unassign Sales Order from Handling Unit (Activity Code: P2)

#### Step 1: Input Validation and Transaction Creation
1. User enters Activity Code = P2, Handling Units (max 1000), Sales Order Number, and Storage Location
2. System validates (same as Part 1):
   - Activity code is P2
   - Handling Unit validation against `/SCWM/AQUA`
   - Sales Order validation via `VBAK` and `VBAP`
   - Call Function Module `ZSCM_SO_SLOC_VALIDATION`
3. Create transaction records in `ZPALTRFHDR` and `ZPALTRFITM` tables with Activity = 'P2'

#### Step 2: Batch Processing - Unassignment
1. Same batch processing logic as Part 1 (split into sets of 10)
2. Call Function Modules `ZSCM_BIN_INTL_POST_GET_DET` and `ZSCM_BIN_INTL_POSTING`
3. Action Code: P2 (used in Function Module calls)
4. Update Status: Running → Completed (or Failed)

### Part 3: SFG to FG Conversion (Activity Code: A2)

#### Step 1: Input Validation
1. User enters Activity Code = A2, Handling Units, Warehouse Number, Storage Location, and Plant
2. System validates:
   - Activity code is A2
   - Warehouse Number exists and is valid
   - Storage Location exists and is valid
   - Plant exists and is valid
   - Handling Units exist in `/SCWM/AQUA`
   - User authorization for conversion operations

#### Step 2: Transaction Creation
1. Create transaction records in `ZPALTRFHDR` and `ZPALTRFITM` tables with Activity = 'A2'
2. Set Status = 'PENDING'

#### Step 3: Conversion Processing
1. Process Handling Units in batches of 10
2. Execute SFG to FG conversion logic
3. Update Status accordingly:
   - Running → Completed (on success)
   - Running → Failed (on error)

## Errors

| Error Code | Error Message | Trigger Condition |
|------------|---------------|-------------------|
| VAL-001 | Maximum handling unit limit exceeded. Maximum 1000 handling units allowed. | Input contains more than 1000 handling units |
| VAL-002 | Invalid handling unit format: {HU_Number} | Handling unit format validation fails |
| VAL-003 | Handling unit does not exist in EWM: {HU_Number} | Handling unit not found in `/SCWM/AQUA` table |
| VAL-004 | Duplicate handling unit found: {HU_Number} | Same handling unit appears multiple times in input |
| VAL-005 | Invalid sales order: {SalesOrder} | Sales order does not exist in `VBAK` or `VBAP` tables |
| VAL-006 | Invalid storage location: {StorageLocation} | Storage location does not exist or is invalid |
| VAL-007 | Invalid warehouse number: {Warehouse} | Warehouse number does not exist or is invalid |
| VAL-008 | Invalid plant: {Plant} | Plant does not exist or is invalid |
| VAL-009 | User not authorized for goods movement | User lacks required authorization |
| VAL-010 | Sales Order and Storage Location validation failed: {ErrorDetails} | Function Module `ZSCM_SO_SLOC_VALIDATION` returns error |
| FM-001 | Function Module ZSCM_BIN_INTL_POST_GET_DET failed for batch {BatchNo}: {ErrorDetails} | Function Module returns error |
| FM-002 | Function Module ZSCM_BIN_INTL_POSTING failed for batch {BatchNo}: {ErrorDetails} | Function Module returns error |
| FM-003 | Function Module timeout for batch {BatchNo} | Function Module call exceeds timeout limit |
| DB-001 | Database error while saving transaction: {ErrorDetails} | Database operation fails |
| DB-002 | Failed to retrieve transaction data: {ErrorDetails} | Database query fails |
| STATUS-001 | Invalid activity code: {ActivityCode} | Activity code is not P1, P2, or A2 |

## Success

- All handling units successfully processed through the workflow
- All transaction records created in `ZPALTRFHDR` and `ZPALTRFITM` tables
- All Function Module calls completed successfully
- All records have Status = 'COMPLETED' in `ZPALTRFHDR` table
- Response structure:
  ```json
  {
    "TransactionID": "0000000001",
    "TotalHandlingUnits": 1000,
    "SuccessfulOperations": 1000,
    "FailedOperations": 0,
    "NumberOfPallets": 500,
    "NumberOfCartons": 2000,
    "Status": "COMPLETED",
    "ProcessingTime": "00:15:30"
  }
  ```

## Edge Cases

- **Partial Success**: Some handling units succeed, others fail
  - Handling: Continue processing remaining batches, log all failures, provide retry mechanism for failed records
  
- **Function Module Timeout**: Function Module times out during batch processing
  - Handling: Mark batch as failed, log timeout error, allow retry of failed batch
  
- **Duplicate Transaction**: Same handling unit processed in multiple transactions
  - Handling: Check existing records in `ZPALTRFHDR`/`ZPALTRFITM` before processing, prevent duplicate processing
  
- **Invalid Sales Order Status**: Sales order exists but is in invalid status for assignment
  - Handling: Validate sales order status via `VBAK`, reject with appropriate error message
  
- **Handling Unit Already Assigned**: Handling unit already has sales order assigned (for P1)
  - Handling: Check current assignment status, reject or allow override based on business rules
  
- **Handling Unit Not Assigned**: Handling unit has no sales order assigned (for P2)
  - Handling: Check assignment status, reject unassignment if not assigned
  
- **Network Interruption**: Connection lost during batch processing
  - Handling: Save current progress, resume from last successful batch upon reconnection
  
- **Concurrent Processing**: Multiple users processing same handling units
  - Handling: Implement record locking, prevent concurrent processing of same handling units
  
- **Empty Input**: User submits empty handling unit list
  - Handling: Validate and reject with appropriate error message
  
- **Invalid Activity Code**: User enters invalid activity code
  - Handling: Validate activity code, reject with error STATUS-001

## Dependencies

- **External Services**:
  - EWM System (Extended Warehouse Management)
  - `/SCWM/AQUA` table (Handling Unit master data)
  - `VBAK` table (Sales Order Header)
  - `VBAP` table (Sales Order Items)

- **Function Modules**:
  - `ZSCM_SO_SLOC_VALIDATION` - Validates Sales Order and Storage Location
  - `ZSCM_BIN_INTL_POST_GET_DET` - Gets posting details for batch processing
  - `ZSCM_BIN_INTL_POSTING` - Executes posting for handling units

- **Databases**:
  - `ZPALTRFHDR` (Custom header table for transaction tracking)
    - Transaction ID (Primary Key)
    - Number of Pallets
    - Created By / Date / Time
    - Activity (P1, P2, A2)
    - Status
  - `ZPALTRFITM` (Custom item table for transaction details)
    - Transaction ID (Foreign Key)
    - Pallet Number
    - Carton Number
  - EWM Database (for handling unit, pallet, carton information)

- **Events**:
  - Posting Completion Event (from `ZSCM_BIN_INTL_POSTING`)
  - Validation Completion Event (from `ZSCM_SO_SLOC_VALIDATION`)

- **Other**:
  - User Authorization Service (for validating user permissions)
  - Transaction Logging Service
  - Error Notification Service

## Performance

- **Max Latency**: 
  - Input validation: < 5 seconds for 1000 handling units
  - Batch processing: < 30 seconds per batch (10 handling units)
  - Total processing time: < 30 minutes for 1000 handling units (assuming 100 batches)

- **Throughput**: 
  - 10 handling units per batch to reduce system performance impact
  - Maximum 100 batches per transaction (1000 handling units)
  - Batch processing interval: Sequential (one batch at a time)

- **Resource Constraints**:
  - Maximum 1000 handling units per transaction to prevent system overload
  - Batch size limited to 10 handling units to optimize Function Module performance
  - Database transaction management for batch commits
  - Memory management for large data sets

## Security

- **Authentication**: 
  - Required: User must be authenticated in SAP system
  - Session-based authentication using SAP logon credentials

- **Authorization**: 
  - User must have authorization for:
    - Goods movement (M_MATE_WRK)
    - Handling unit operations
    - EWM warehouse operations
    - Custom transaction execution (ZPALTRFHDR/ZPALTRFITM)
  - Authorization check before processing each batch

- **PII Masking**: 
  - No PII data involved in this process
  - User ID logged for audit purposes

- **Data Encryption**: 
  - Database connections must use encrypted channels
  - Function Module calls must use secure communication protocols
  - Transaction data stored with standard SAP security measures

- **Audit Trail**:
  - All transactions logged with user ID, timestamp, and status
  - Error logs maintained for troubleshooting and compliance
  - Transaction history maintained in `ZPALTRFHDR` and `ZPALTRFITM` tables

## Tests

- **AT-001**: Happy Path - Assign Sales Order to 10 Handling Units successfully
  - Input: Activity P1, 10 valid handling units, valid sales order, valid storage location
  - Expected: All handling units processed, Status = COMPLETED

- **AT-002**: Maximum Input - Process 1000 handling units successfully
  - Input: 1000 valid handling units
  - Expected: All 100 batches processed successfully, Status = COMPLETED

- **AT-003**: Invalid Handling Unit - Input contains invalid handling unit
  - Input: 9 valid + 1 invalid handling unit
  - Expected: Validation error VAL-003, transaction rejected before processing

- **AT-004**: Duplicate Handling Units - Input contains duplicate handling units
  - Input: Same handling unit entered twice
  - Expected: Validation error VAL-004, transaction rejected

- **AT-005**: Invalid Sales Order - Sales order does not exist
  - Input: Valid handling units, invalid sales order
  - Expected: Validation error VAL-005, transaction rejected

- **AT-006**: Function Module Failure - Function Module fails for one batch
  - Input: 20 handling units (2 batches), Function Module fails for batch 2
  - Expected: Batch 1 succeeds, Batch 2 fails, partial success status, error logged

- **AT-007**: Unassign Sales Order - Unassign from 10 handling units
  - Input: Activity P2, 10 valid handling units with assigned sales order
  - Expected: All handling units unassigned successfully, Status = COMPLETED

- **AT-008**: SFG to FG Conversion - Convert 10 handling units
  - Input: Activity A2, 10 valid handling units, valid warehouse/SLOC/plant
  - Expected: All conversions successful, Status = COMPLETED

- **AT-009**: Empty Input - User submits empty handling unit list
  - Input: No handling units
  - Expected: Validation error, transaction rejected

- **AT-010**: Authorization Failure - User lacks required authorization
  - Input: Valid handling units, user without goods movement authorization
  - Expected: Authorization error VAL-009, transaction rejected

- **AT-011**: Concurrent Processing - Multiple users process same handling unit
  - Input: Same handling unit in two concurrent transactions
  - Expected: Second transaction detects duplicate, prevents processing or handles gracefully

- **AT-012**: Invalid Activity Code - User enters invalid activity code
  - Input: Activity code = 'XX'
  - Expected: Validation error STATUS-001, transaction rejected

- **AT-013**: Sales Order Validation Failure - Function Module validation fails
  - Input: Valid handling units, sales order fails `ZSCM_SO_SLOC_VALIDATION`
  - Expected: Validation error VAL-010, transaction rejected

## Database Schema

### ZPALTRFHDR (Header Table)

| Field Name | Type | Length | Key | Description |
|------------|------|--------|-----|-------------|
| TRANSACTION_ID | NUMC | 10 | Primary Key | Auto-generated, auto-incremental transaction identifier |
| NUM_PALLETS | INT4 | 10 | | Number of pallets in transaction |
| CREATED_BY | CHAR | 12 | | User ID who created the transaction |
| CREATED_DATE | DATS | 8 | | Date of transaction creation |
| CREATED_TIME | TIMS | 6 | | Time of transaction creation |
| ACTIVITY | CHAR | 2 | | Activity code: P1, P2, A2 |
| STATUS | CHAR | 20 | | Status: PENDING, RUNNING, COMPLETED, FAILED, PARTIAL_SUCCESS |

### ZPALTRFITM (Item Table)

| Field Name | Type | Length | Key | Description |
|------------|------|--------|-----|-------------|
| TRANSACTION_ID | NUMC | 10 | Primary Key (Part 1) | Transaction identifier (Foreign Key to ZPALTRFHDR) |
| PALLET_NUMBER | CHAR | 20 | Primary Key (Part 2) | Pallet number from handling unit |
| CARTON_NUMBER | CHAR | 20 | Primary Key (Part 3) | Carton number associated with pallet |

## Function Module Interface

### IMPORTING Parameters

| Parameter Name | Type | Required | Description |
|----------------|------|----------|-------------|
| IV_ACTIVITY_CODE | CHAR(2) | Yes | Activity code: P1 (Assign), P2 (Unassign), A2 (SFG to FG Conversion) |
| IT_HANDLING_UNITS | Table | Yes | Table of handling units (max 1000 entries) |
| IV_SALES_ORDER | VBELN | Conditional | Sales Order Number (required for P1, P2) |
| IV_STORAGE_LOCATION | LGORT | Conditional | Storage Location (required for P1, P2) |
| IV_WAREHOUSE | LGNUM | Conditional | Warehouse Number (required for A2) |
| IV_PLANT | WERKS_D | Conditional | Plant Code (required for A2) |

### EXPORTING Parameters

| Parameter Name | Type | Description |
|----------------|------|-------------|
| EV_TRANSACTION_ID | NUMC(10) | Auto-generated transaction identifier |
| EV_TOTAL_HUS | INT4 | Total handling units processed |
| EV_SUCCESS_COUNT | INT4 | Count of successful operations |
| EV_FAIL_COUNT | INT4 | Count of failed operations |
| EV_STATUS | CHAR(20) | Overall processing status |
| EV_NUM_PALLETS | INT4 | Total number of pallets processed |
| EV_NUM_CARTONS | INT4 | Total number of cartons processed |

### TABLES Parameters

| Parameter Name | Type | Description |
|----------------|------|-------------|
| ET_BATCH_DETAILS | Table | Details of each batch processed |
| ET_ERROR_LOG | Table | Detailed error log |
| ET_MESSAGES | Table | Return messages (BAPIRET2 format) |

### EXCEPTIONS

| Exception Name | Description |
|----------------|-------------|
| INVALID_ACTIVITY_CODE | Activity code is not P1, P2, or A2 |
| INVALID_HANDLING_UNIT | Handling unit validation failed |
| INVALID_SALES_ORDER | Sales order validation failed |
| INVALID_STORAGE_LOCATION | Storage location validation failed |
| INVALID_WAREHOUSE | Warehouse validation failed |
| INVALID_PLANT | Plant validation failed |
| NO_AUTHORITY | User lacks required authorization |
| MAX_LIMIT_EXCEEDED | More than 1000 handling units provided |
| DUPLICATE_HANDLING_UNITS | Duplicate handling units found in input |
| FUNCTION_MODULE_ERROR | Error in called function modules |
| DATABASE_ERROR | Database operation failed |
| VALIDATION_ERROR | General validation error |

## Business Rules

1. Maximum 1000 handling units can be processed in a single transaction
2. Batch size is fixed at 10 handling units to optimize Function Module performance
3. Processing is sequential (one batch at a time)
4. Failed batches do not stop processing of subsequent batches
5. All successful operations must have corresponding status updates
6. Transaction status must be updated at each processing stage
7. All errors must be logged with detailed information
8. User can retry failed records individually or in batch
9. Transaction history must be maintained for audit purposes
10. Activity codes are mutually exclusive: P1 (Assign), P2 (Unassign), A2 (SFG to FG Conversion)
11. Sales Order validation is mandatory for P1 and P2 activities
12. Warehouse/SLOC/Plant validation is mandatory for A2 activity

