*&---------------------------------------------------------------------*
*& Report ZSALARY_ADVANCE_REPORT
*& Transaction: ZSALADVREP
*& Description: Salary Advance Report
*& Status: Testing Pending
*&---------------------------------------------------------------------*
REPORT zsalary_advance_report.

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: pa0001,  " HR Master Record: Infotype 0001 (Org. Assignment)
        pa0000,  " HR Master Record: Infotype 0000 (Actions)
        pa0015.  " HR Master Record: Infotype 0015 (Additional Payments)

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_salary_advance,
         pernr         TYPE pa0001-pernr,      " Personnel Number
         ename         TYPE pa0001-ename,      " Employee Name
         bukrs         TYPE pa0001-bukrs,      " Company Code
         werks         TYPE pa0001-werks,      " Personnel Area
         kostl         TYPE pa0001-kostl,      " Cost Center
         stat2         TYPE pa0000-stat2,      " Employment Status
         advance_date  TYPE pa0015-begda,      " Advance Date
         lgart         TYPE pa0015-lgart,      " Wage Type
         advance_amt   TYPE p DECIMALS 2,      " Advance Amount
         deducted_amt  TYPE p DECIMALS 2,      " Deducted Amount
         balance       TYPE p DECIMALS 2,      " Outstanding Balance
         status        TYPE c LENGTH 10,       " Status
       END OF ty_salary_advance.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_advances  TYPE TABLE OF ty_salary_advance,
      gs_advance   TYPE ty_salary_advance,
      gv_total_adv TYPE p DECIMALS 2,
      gv_total_bal TYPE p DECIMALS 2.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_pernr FOR pa0001-pernr,      " Personnel Number
                  s_bukrs FOR pa0001-bukrs,      " Company Code
                  s_werks FOR pa0001-werks,      " Personnel Area
                  s_kostl FOR pa0001-kostl.      " Cost Center
  PARAMETERS:     p_date  TYPE sy-datum DEFAULT sy-datum.  " Key Date
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_begda FOR pa0015-begda.      " Advance Date Range
  SELECT-OPTIONS: s_lgart FOR pa0015-lgart.      " Wage Types
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_active TYPE c AS CHECKBOX DEFAULT 'X'.  " Active Only
  PARAMETERS: p_outst  TYPE c AS CHECKBOX DEFAULT 'X'.  " Outstanding Only
  PARAMETERS: p_all    TYPE c AS CHECKBOX DEFAULT ' '.  " Show All
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'Selection Criteria'.
  TEXT-002 = 'Advance Filters'.
  TEXT-003 = 'Status Filters'.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_advances.
  PERFORM calculate_balances.
  PERFORM filter_results.
  PERFORM display_report.

*&---------------------------------------------------------------------*
*& Form SELECT_ADVANCES
*&---------------------------------------------------------------------*
FORM select_advances.

  SELECT a~pernr a~ename a~bukrs a~werks a~kostl
         b~stat2
         c~begda AS advance_date c~lgart c~betrg AS advance_amt
    INTO CORRESPONDING FIELDS OF TABLE gt_advances
    FROM pa0001 AS a
    INNER JOIN pa0000 AS b ON a~pernr = b~pernr
    INNER JOIN pa0015 AS c ON a~pernr = c~pernr
    WHERE a~pernr IN s_pernr
      AND a~bukrs IN s_bukrs
      AND a~werks IN s_werks
      AND a~kostl IN s_kostl
      AND c~begda IN s_begda
      AND c~lgart IN s_lgart
      AND p_date BETWEEN a~begda AND a~endda
      AND p_date BETWEEN b~begda AND b~endda.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALCULATE_BALANCES
*&---------------------------------------------------------------------*
FORM calculate_balances.

  LOOP AT gt_advances INTO gs_advance.
*   Get deducted amount from payroll results
*   This is placeholder - actual would read from payroll cluster
    gs_advance-deducted_amt = 0.

*   Calculate balance
    gs_advance-balance = gs_advance-advance_amt - gs_advance-deducted_amt.

*   Set status
    IF gs_advance-balance <= 0.
      gs_advance-status = 'SETTLED'.
    ELSEIF gs_advance-balance = gs_advance-advance_amt.
      gs_advance-status = 'PENDING'.
    ELSE.
      gs_advance-status = 'PARTIAL'.
    ENDIF.

    MODIFY gt_advances FROM gs_advance.

*   Update totals
    gv_total_adv = gv_total_adv + gs_advance-advance_amt.
    gv_total_bal = gv_total_bal + gs_advance-balance.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILTER_RESULTS
*&---------------------------------------------------------------------*
FORM filter_results.

  IF p_all <> 'X'.
*   Filter by active status
    IF p_active = 'X'.
      DELETE gt_advances WHERE stat2 <> '3'.
    ENDIF.

*   Filter outstanding only
    IF p_outst = 'X'.
      DELETE gt_advances WHERE balance <= 0.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_REPORT
*&---------------------------------------------------------------------*
FORM display_report.

  DATA: lo_alv TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_advances ).

      lo_alv->display( ).

    CATCH cx_salv_msg.
      PERFORM display_simple.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_SIMPLE
*&---------------------------------------------------------------------*
FORM display_simple.

  WRITE: / 'Salary Advance Report'.
  WRITE: / 'Date:', p_date.
  ULINE.

  WRITE: / 'Pernr', 12 'Name', 35 'Date', 48 'Advance', 62 'Balance', 76 'Status'.
  ULINE.

  LOOP AT gt_advances INTO gs_advance.
    WRITE: / gs_advance-pernr,
             12 gs_advance-ename,
             35 gs_advance-advance_date,
             48 gs_advance-advance_amt,
             62 gs_advance-balance,
             76 gs_advance-status.
  ENDLOOP.

  ULINE.
  WRITE: / 'Total Advances:', 48 gv_total_adv.
  WRITE: / 'Total Outstanding:', 62 gv_total_bal.

ENDFORM.
