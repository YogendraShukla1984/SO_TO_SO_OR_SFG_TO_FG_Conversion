
# FS for Solar Activity

A new program to be created to assign/unassign sales order to Handling Unit and transfer handling unit to SLOC.

## Part 1: Assign Sales Order to Handling Unit

### Data Validation
- Validate Handling Units against `/SCWM/AQUA`
- Validate Sales Order via `VBAK` and `VBAP`
- FM: `ZSCM_SO_SLOC_VALIDATION`

### Table Updates
**Header Table: ZPALTRFHDR**
- Transaction ID
- Number of Pallets
- Created By / Date / Time
- Activity

**Item Table: ZPALTRFITM**
- Transaction ID
- Pallet Number
- Carton Number

### Processing Logic
- Split Handling Units into sets of 10
- FM Calls:
  - `ZSCM_BIN_INTL_POST_GET_DET`
  - `ZSCM_BIN_INTL_POSTING`
- Status update: Running → Completed

## Part 2: Unassign Sales Order from Handling Unit
- Same validation and table logic as Part 1
- Action Code: P2

## Part 3: SFG to FG Conversion
- Validate Warehouse, SLOC, Plant
- Action Code: A2
- Update Status accordingly
