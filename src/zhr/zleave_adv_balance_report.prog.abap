*&---------------------------------------------------------------------*
*& Report ZLEAVE_ADV_BALANCE_REPORT
*& Transaction: ZLEAADVREP
*& Description: Leave Advance Balance Report
*& Feedback: Shows all employees with leave advance /693
*&           Should check offcycle RT, inactive employee and payment
*& Status: Working
*&---------------------------------------------------------------------*
REPORT zleave_adv_balance_report.

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: pa0001,  " HR Master Record: Infotype 0001 (Org. Assignment)
        pa0000,  " HR Master Record: Infotype 0000 (Actions)
        pa0014.  " HR Master Record: Infotype 0014 (Recurring Payments)

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: c_wagetype_leave_adv TYPE pa0014-lgart VALUE '/693'.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_leave_advance,
         pernr        TYPE pa0001-pernr,      " Personnel Number
         ename        TYPE pa0001-ename,      " Employee Name
         bukrs        TYPE pa0001-bukrs,      " Company Code
         werks        TYPE pa0001-werks,      " Personnel Area
         kostl        TYPE pa0001-kostl,      " Cost Center
         stat2        TYPE pa0000-stat2,      " Employment Status
         advance_amt  TYPE p DECIMALS 2,      " Leave Advance Amount
         deducted_amt TYPE p DECIMALS 2,      " Deducted Amount
         balance      TYPE p DECIMALS 2,      " Outstanding Balance
         offcycle_rt  TYPE c LENGTH 1,        " Offcycle RT Flag
         payment_stat TYPE c LENGTH 10,       " Payment Status
       END OF ty_leave_advance.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_advances   TYPE TABLE OF ty_leave_advance,
      gs_advance    TYPE ty_leave_advance,
      gv_total_bal  TYPE p DECIMALS 2.

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
  PARAMETERS: p_active TYPE c AS CHECKBOX DEFAULT ' '.  " Active Only
  PARAMETERS: p_inactv TYPE c AS CHECKBOX DEFAULT 'X'.  " Include Inactive
  PARAMETERS: p_offcyc TYPE c AS CHECKBOX DEFAULT 'X'.  " Check Offcycle RT
  PARAMETERS: p_zero   TYPE c AS CHECKBOX DEFAULT ' '.  " Include Zero Balance
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'Selection Criteria'.
  TEXT-002 = 'Status Filters'.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_employees.
  PERFORM get_leave_advances.
  PERFORM check_offcycle_rt.
  PERFORM filter_results.
  PERFORM display_report.

*&---------------------------------------------------------------------*
*& Form SELECT_EMPLOYEES
*&---------------------------------------------------------------------*
FORM select_employees.

  SELECT a~pernr a~ename a~bukrs a~werks a~kostl b~stat2
    INTO CORRESPONDING FIELDS OF TABLE gt_advances
    FROM pa0001 AS a
    INNER JOIN pa0000 AS b ON a~pernr = b~pernr
    WHERE a~pernr IN s_pernr
      AND a~bukrs IN s_bukrs
      AND a~werks IN s_werks
      AND a~kostl IN s_kostl
      AND p_date BETWEEN a~begda AND a~endda
      AND p_date BETWEEN b~begda AND b~endda.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_LEAVE_ADVANCES
*&---------------------------------------------------------------------*
FORM get_leave_advances.

  DATA: lt_pa0014 TYPE TABLE OF pa0014,
        ls_pa0014 TYPE pa0014.

  LOOP AT gt_advances INTO gs_advance.
*   Get leave advance wage type /693
    SELECT * FROM pa0014 INTO TABLE lt_pa0014
      WHERE pernr = gs_advance-pernr
        AND lgart = c_wagetype_leave_adv
        AND p_date BETWEEN begda AND endda.

    LOOP AT lt_pa0014 INTO ls_pa0014.
      gs_advance-advance_amt = gs_advance-advance_amt + ls_pa0014-betrg.
    ENDLOOP.

*   Calculate balance (advance - deducted)
    gs_advance-balance = gs_advance-advance_amt - gs_advance-deducted_amt.

    MODIFY gt_advances FROM gs_advance.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_OFFCYCLE_RT
*&---------------------------------------------------------------------*
FORM check_offcycle_rt.

  IF p_offcyc = 'X'.
    LOOP AT gt_advances INTO gs_advance.
*     Check for offcycle payroll results
*     This would need to read payroll cluster RT
*     Placeholder logic
      gs_advance-offcycle_rt = ' '.

*     Set payment status
      IF gs_advance-balance > 0.
        gs_advance-payment_stat = 'PENDING'.
      ELSE.
        gs_advance-payment_stat = 'SETTLED'.
      ENDIF.

      MODIFY gt_advances FROM gs_advance.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILTER_RESULTS
*&---------------------------------------------------------------------*
FORM filter_results.

* Filter by active status
  IF p_active = 'X' AND p_inactv <> 'X'.
    DELETE gt_advances WHERE stat2 <> '3'.
  ENDIF.

* Remove zero balances
  IF p_zero <> 'X'.
    DELETE gt_advances WHERE balance = 0.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_REPORT
*&---------------------------------------------------------------------*
FORM display_report.

  DATA: lo_alv TYPE REF TO cl_salv_table.

* Calculate total
  LOOP AT gt_advances INTO gs_advance.
    gv_total_bal = gv_total_bal + gs_advance-balance.
  ENDLOOP.

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

  WRITE: / 'Leave Advance Balance Report'.
  WRITE: / 'Date:', p_date.
  WRITE: / 'Wage Type:', c_wagetype_leave_adv.
  ULINE.

  WRITE: / 'Pernr', 12 'Name', 35 'Status', 45 'Advance', 60 'Balance'.
  ULINE.

  LOOP AT gt_advances INTO gs_advance.
    WRITE: / gs_advance-pernr,
             12 gs_advance-ename,
             35 gs_advance-stat2,
             45 gs_advance-advance_amt,
             60 gs_advance-balance.
  ENDLOOP.

  ULINE.
  WRITE: / 'Total Outstanding Balance:', 60 gv_total_bal.

ENDFORM.
