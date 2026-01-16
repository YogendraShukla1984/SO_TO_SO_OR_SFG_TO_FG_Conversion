*&---------------------------------------------------------------------*
*& Include          LZSCM_SFG_CONVERSIONTOP
*&---------------------------------------------------------------------*
*& Function Group: ZSCM_SFG_CONVERSION
*& Purpose: SFG to FG Conversion Tracking - Global Declarations
*& Author: System
*& Creation Date: 2025-12-31
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*

" Activity type
TYPES: BEGIN OF gty_activity,
         activity TYPE char20,
       END OF gty_activity.

TYPES: gty_t_activity TYPE STANDARD TABLE OF gty_activity.

" Pallet number
TYPES: BEGIN OF gty_pallet,
         pallet_no TYPE char20,
       END OF gty_pallet.

TYPES: gty_t_pallet TYPE STANDARD TABLE OF gty_pallet.

" Plant
TYPES: BEGIN OF gty_plant,
         werks TYPE werks_d,
       END OF gty_plant.

TYPES: gty_t_plant TYPE STANDARD TABLE OF gty_plant.

" Output structure for main report
TYPES: BEGIN OF gty_report_output,
         transaction_id      TYPE char20,
         transaction_date    TYPE dats,
         handling_unit       TYPE char20,
         product             TYPE char18,
         product_desc        TYPE char40,
         transfer_start_date TYPE dats,
         transfer_start_time TYPE tims,
         fg_receipt_date     TYPE dats,
         fg_receipt_time     TYPE tims,
         plant               TYPE werks_d,
         from_sloc           TYPE lgort_d,
         to_sloc             TYPE lgort_d,
         from_storage_bin    TYPE char18,
         to_storage_bin      TYPE char18,
         sfg_quantity        TYPE meng13,
         fg_quantity         TYPE meng13,
         base_uom            TYPE meins,
         batch               TYPE charg_d,
         stock_type          TYPE char2,
         sales_order         TYPE vbeln_vl,
         so_item             TYPE posnr_vl,
         user_id             TYPE syuname,
         pallet_prod_date    TYPE dats,
         pallet_grn_date     TYPE dats,
         conversion_status   TYPE char30,
         error_details       TYPE char255,
       END OF gty_report_output.

TYPES: gty_t_report_output TYPE STANDARD TABLE OF gty_report_output.

" Status structure
TYPES: BEGIN OF gty_status,
         pallet_no TYPE char20,
         status    TYPE char30,
         message   TYPE char255,
       END OF gty_status.

TYPES: gty_t_status TYPE STANDARD TABLE OF gty_status.

" Transaction header structure (matching SELECT fields)
TYPES: BEGIN OF gty_trfhdr,
         transaction_id TYPE char20,
         created_on     TYPE dats,
         activity_type  TYPE char20,
       END OF gty_trfhdr.

TYPES: gty_t_trfhdr TYPE STANDARD TABLE OF gty_trfhdr.

" Transaction item structure (matching SELECT fields)
TYPES: BEGIN OF gty_trfitm,
         transaction_id TYPE char20,
         pallet_number  TYPE char20,
         item_no        TYPE numc4,
       END OF gty_trfitm.

TYPES: gty_t_trfitm TYPE STANDARD TABLE OF gty_trfitm.

" Product info from /SCWM/AQUA (matching SELECT fields)
TYPES: BEGIN OF gty_aqua,
         hu_number       TYPE char20,
         matid           TYPE char18,
         quan            TYPE meng13,
         altme           TYPE meins,
         charg           TYPE charg_d,
         cat             TYPE char2,
         stock_docno     TYPE vbeln_vl,
         stock_itemno    TYPE posnr_vl,
         lgpla           TYPE char18,
         wdatu           TYPE dats,
       END OF gty_aqua.

TYPES: gty_t_aqua TYPE STANDARD TABLE OF gty_aqua.

" Material key (matching SELECT fields)
TYPES: BEGIN OF gty_matkey,
         matid  TYPE char18,
         maktx  TYPE char40,
       END OF gty_matkey.

TYPES: gty_t_matkey TYPE STANDARD TABLE OF gty_matkey.

" Stock details from YSTKDTLS (matching SELECT fields)
TYPES: BEGIN OF gty_stkdtls,
         usrno       TYPE char20,
         vbeln       TYPE vbeln_vl,
         created_on  TYPE dats,
         created_tm  TYPE tims,
         sourcewerks TYPE werks_d,
         tube_m      TYPE char18,
         created_by  TYPE syuname,
         mblnr       TYPE mblnr,
         mjahr       TYPE mjahr,
       END OF gty_stkdtls.

TYPES: gty_t_stkdtls TYPE STANDARD TABLE OF gty_stkdtls.

" Material document from MSEG (matching SELECT fields)
TYPES: BEGIN OF gty_mseg,
         mblnr     TYPE mblnr,
         mjahr     TYPE mjahr,
         zeile     TYPE mblpo,
         matnr     TYPE matnr,
         werks     TYPE werks_d,
         lgort     TYPE lgort_d,
         charg     TYPE charg_d,
       END OF gty_mseg.

TYPES: gty_t_mseg TYPE STANDARD TABLE OF gty_mseg.

" Material document header from MKPF (matching SELECT fields)
TYPES: BEGIN OF gty_mkpf,
         mblnr TYPE mblnr,
         mjahr TYPE mjahr,
         cpudt TYPE cpudt,
         cputm TYPE cputm,
       END OF gty_mkpf.

TYPES: gty_t_mkpf TYPE STANDARD TABLE OF gty_mkpf.

" VBUK structure (matching SELECT fields)
TYPES: BEGIN OF gty_vbuk,
         vbeln TYPE vbeln_vl,
         wbstk TYPE char1,
       END OF gty_vbuk.

TYPES: gty_t_vbuk TYPE STANDARD TABLE OF gty_vbuk.

" LIKP structure (matching SELECT fields)
TYPES: BEGIN OF gty_likp,
         vbeln TYPE vbeln_vl,
         vlstk TYPE char1,
       END OF gty_likp.

TYPES: gty_t_likp TYPE STANDARD TABLE OF gty_likp.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: gc_dest_ecc           TYPE rfcdest VALUE 'ECC_DEST',
           gc_status_putaway     TYPE char30 VALUE 'PUTAWAY_DONE',
           gc_status_ibd_pending TYPE char30 VALUE 'IBD_DISTRIB_PENDING',
           gc_status_ibd_create  TYPE char30 VALUE 'IBD_CREATION_PENDING',
           gc_status_not_posted  TYPE char30 VALUE 'CONV_DOC_NOT_POSTED'.

" END: Cursor Generated Code


