FUNCTION zscm_sfg_conversion_status.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_PALLET_NO) TYPE  GTY_T_PALLET
*"  EXPORTING
*"     REFERENCE(ET_STATUS) TYPE  GTY_T_STATUS
*"----------------------------------------------------------------------
  " BEGIN: Cursor Generated Code

  " Data declarations (7.31 compliance)
  DATA: lt_ystkdtls TYPE STANDARD TABLE OF ystkdtls,
        lw_ystkdtls TYPE ystkdtls,
        lt_vbuk     TYPE gty_t_vbuk,
        lw_vbuk     TYPE gty_vbuk,
        lt_likp     TYPE gty_t_likp,
        lw_likp     TYPE gty_likp,
        lw_pallet   TYPE gty_pallet,
        lw_status   TYPE gty_status,
        lt_vbeln    TYPE STANDARD TABLE OF vbeln_vl,
        lv_vbeln    TYPE vbeln_vl.

  " Validate input
  IF it_pallet_no IS INITIAL.
    RETURN.
  ENDIF.

  " Retrieve YSTKDTLS records for all pallets via RFC
  TRY.
      CALL FUNCTION 'Z_GET_YSTKDTLS_DATA'
        DESTINATION gc_dest_ecc
        EXPORTING
          it_pallet_no = it_pallet_no
        IMPORTING
          et_ystkdtls  = lt_ystkdtls
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          OTHERS                = 3.

      IF sy-subrc <> 0.
        " If RFC fails, return error status for all pallets
        LOOP AT it_pallet_no INTO lw_pallet.
          CLEAR: lw_status.
          lw_status-pallet_no = lw_pallet-pallet_no.
          lw_status-status = 'ERROR'.
          lw_status-message = 'Unable to retrieve data from ECC system'.
          APPEND lw_status TO et_status.
        ENDLOOP.
        RETURN.
      ENDIF.

    CATCH cx_root.
      " Exception handling
      LOOP AT it_pallet_no INTO lw_pallet.
        CLEAR: lw_status.
        lw_status-pallet_no = lw_pallet-pallet_no.
        lw_status-status = 'ERROR'.
        lw_status-message = 'System error accessing ECC data'.
        APPEND lw_status TO et_status.
      ENDLOOP.
      RETURN.
  ENDTRY.

  " Sort for binary search
  SORT lt_ystkdtls BY usrno.

  " Collect all IBD numbers for batch retrieval
  LOOP AT lt_ystkdtls INTO lw_ystkdtls WHERE vbeln IS NOT INITIAL.
    lv_vbeln = lw_ystkdtls-vbeln.
    APPEND lv_vbeln TO lt_vbeln.
  ENDLOOP.

  IF lt_vbeln IS NOT INITIAL.
    SORT lt_vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln.

    " Get VBUK records for all IBDs
    SELECT vbeln wbstk
      FROM vbuk
      INTO TABLE lt_vbuk
      FOR ALL ENTRIES IN lt_vbeln
      WHERE vbeln = lt_vbeln-table_line.

    IF sy-subrc = 0.
      SORT lt_vbuk BY vbeln.
    ENDIF.

    " Get LIKP records for all IBDs
    SELECT vbeln vlstk
      FROM likp
      INTO TABLE lt_likp
      FOR ALL ENTRIES IN lt_vbeln
      WHERE vbeln = lt_vbeln-table_line.

    IF sy-subrc = 0.
      SORT lt_likp BY vbeln.
    ENDIF.
  ENDIF.

  " Process each pallet
  LOOP AT it_pallet_no INTO lw_pallet.
    CLEAR: lw_status, lw_ystkdtls.

    lw_status-pallet_no = lw_pallet-pallet_no.

    " Check if pallet exists in YSTKDTLS
    READ TABLE lt_ystkdtls INTO lw_ystkdtls
      WITH KEY usrno = lw_pallet-pallet_no
      BINARY SEARCH.

    IF sy-subrc <> 0.
      " Pallet not found in YSTKDTLS
      lw_status-status = gc_status_not_posted.
      lw_status-message = 'Conversion document not posted'.
      APPEND lw_status TO et_status.
      CONTINUE.
    ENDIF.

    " Check if IBD exists
    IF lw_ystkdtls-vbeln IS INITIAL.
      lw_status-status = gc_status_ibd_create.
      lw_status-message = 'IBD creation Pending'.
      APPEND lw_status TO et_status.
      CONTINUE.
    ENDIF.

    " Check VBUK for putaway status
    CLEAR: lw_vbuk.
    READ TABLE lt_vbuk INTO lw_vbuk
      WITH KEY vbeln = lw_ystkdtls-vbeln
      BINARY SEARCH.

    IF sy-subrc = 0.
      IF lw_vbuk-wbstk = 'C'.
        lw_status-status = gc_status_putaway.
        lw_status-message = 'Putaway done'.
        APPEND lw_status TO et_status.
        CONTINUE.
      ENDIF.
    ENDIF.

    " Check LIKP for distribution status
    CLEAR: lw_likp.
    READ TABLE lt_likp INTO lw_likp
      WITH KEY vbeln = lw_ystkdtls-vbeln
      BINARY SEARCH.

    IF sy-subrc = 0.
      IF lw_likp-vlstk = 'B'.
        lw_status-status = gc_status_ibd_pending.
        lw_status-message = 'IBD Distribution Pending'.
        APPEND lw_status TO et_status.
        CONTINUE.
      ENDIF.
    ENDIF.

    " Default status
    lw_status-status = gc_status_ibd_pending.
    lw_status-message = 'IBD in process'.
    APPEND lw_status TO et_status.
  ENDLOOP.

  " END: Cursor Generated Code
ENDFUNCTION.


