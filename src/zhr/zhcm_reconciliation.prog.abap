*&---------------------------------------------------------------------*
*& Report ZHCM_RECONCILIATION
*& Transaction: ZRECONREP
*& Description: HCM Reconciliation Report
*& Feedback: Previous 559 and current 559, difference in 552
*&           Need to recheck details of 552cc(3112)
*& Status: Working
*&---------------------------------------------------------------------*
REPORT zhcm_reconciliation.

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: pa0001,  " HR Master Record: Infotype 0001 (Org. Assignment)
        t512w.   " Wage Types

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_reconciliation,
         pernr        TYPE pa0001-pernr,      " Personnel Number
         ename        TYPE pa0001-ename,      " Employee Name
         bukrs        TYPE pa0001-bukrs,      " Company Code
         werks        TYPE pa0001-werks,      " Personnel Area
         kostl        TYPE pa0001-kostl,      " Cost Center
         lgart        TYPE char4,             " Wage Type
         prev_amount  TYPE p DECIMALS 2,      " Previous Period Amount
         curr_amount  TYPE p DECIMALS 2,      " Current Period Amount
         difference   TYPE p DECIMALS 2,      " Difference
         variance_pct TYPE p DECIMALS 2,      " Variance Percentage
         status       TYPE c LENGTH 10,       " Status
       END OF ty_reconciliation.

TYPES: BEGIN OF ty_summary,
         lgart       TYPE char4,              " Wage Type
         lgtxt       TYPE t512t-lgtxt,        " Wage Type Text
         prev_total  TYPE p DECIMALS 2,       " Previous Total
         curr_total  TYPE p DECIMALS 2,       " Current Total
         difference  TYPE p DECIMALS 2,       " Total Difference
       END OF ty_summary.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_recon    TYPE TABLE OF ty_reconciliation,
      gs_recon    TYPE ty_reconciliation,
      gt_summary  TYPE TABLE OF ty_summary,
      gs_summary  TYPE ty_summary,
      gv_prev_per TYPE pnppaession,           " Previous Period
      gv_curr_per TYPE pnppaession.           " Current Period

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_pernr FOR pa0001-pernr,      " Personnel Number
                  s_bukrs FOR pa0001-bukrs,      " Company Code
                  s_werks FOR pa0001-werks,      " Personnel Area
                  s_kostl FOR pa0001-kostl.      " Cost Center
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_pyper1 TYPE pnppaession OBLIGATORY.  " Previous Period
  PARAMETERS: p_pyper2 TYPE pnppaession OBLIGATORY.  " Current Period
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: s_lgart FOR t512w-lgart.       " Wage Types
  PARAMETERS:     p_varmin TYPE p DECIMALS 2 DEFAULT 0.  " Min Variance %
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'Selection Criteria'.
  TEXT-002 = 'Payroll Periods'.
  TEXT-003 = 'Wage Type Filters'.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  gv_prev_per = p_pyper1.
  gv_curr_per = p_pyper2.

  PERFORM select_employees.
  PERFORM get_payroll_results.
  PERFORM calculate_variances.
  PERFORM build_summary.
  PERFORM display_report.

*&---------------------------------------------------------------------*
*& Form SELECT_EMPLOYEES
*&---------------------------------------------------------------------*
FORM select_employees.

  DATA: lt_employees TYPE TABLE OF ty_reconciliation.

  SELECT pernr ename bukrs werks kostl
    INTO CORRESPONDING FIELDS OF TABLE lt_employees
    FROM pa0001
    WHERE pernr IN s_pernr
      AND bukrs IN s_bukrs
      AND werks IN s_werks
      AND kostl IN s_kostl
      AND endda = '99991231'.

* Initialize with wage types to compare
  DATA: lt_lgart TYPE TABLE OF t512w-lgart,
        lv_lgart TYPE t512w-lgart.

* Key wage types for reconciliation
  APPEND '559' TO lt_lgart.   " Main wage type
  APPEND '552' TO lt_lgart.   " Difference wage type

  LOOP AT lt_employees INTO gs_recon.
    LOOP AT lt_lgart INTO lv_lgart.
      gs_recon-lgart = lv_lgart.
      APPEND gs_recon TO gt_recon.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_PAYROLL_RESULTS
*&---------------------------------------------------------------------*
FORM get_payroll_results.

* This is placeholder logic
* Actual implementation would read from payroll clusters (PCL2)
* using macros like RP-IMP-C2-xx

  LOOP AT gt_recon INTO gs_recon.
*   Get previous period amount
*   PERFORM read_payroll_cluster USING gs_recon-pernr
*                                      gv_prev_per
*                                      gs_recon-lgart
*                               CHANGING gs_recon-prev_amount.

*   Get current period amount
*   PERFORM read_payroll_cluster USING gs_recon-pernr
*                                      gv_curr_per
*                                      gs_recon-lgart
*                               CHANGING gs_recon-curr_amount.

*   Placeholder values for testing
    gs_recon-prev_amount = 559.
    gs_recon-curr_amount = 559.

    MODIFY gt_recon FROM gs_recon.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALCULATE_VARIANCES
*&---------------------------------------------------------------------*
FORM calculate_variances.

  LOOP AT gt_recon INTO gs_recon.
*   Calculate difference
    gs_recon-difference = gs_recon-curr_amount - gs_recon-prev_amount.

*   Calculate variance percentage
    IF gs_recon-prev_amount <> 0.
      gs_recon-variance_pct = ( gs_recon-difference / gs_recon-prev_amount ) * 100.
    ENDIF.

*   Set status
    IF gs_recon-difference = 0.
      gs_recon-status = 'OK'.
    ELSEIF gs_recon-difference > 0.
      gs_recon-status = 'INCREASE'.
    ELSE.
      gs_recon-status = 'DECREASE'.
    ENDIF.

    MODIFY gt_recon FROM gs_recon.
  ENDLOOP.

* Filter by minimum variance
  IF p_varmin > 0.
    DELETE gt_recon WHERE variance_pct < p_varmin
                      AND variance_pct > ( p_varmin * -1 ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_SUMMARY
*&---------------------------------------------------------------------*
FORM build_summary.

  DATA: lt_lgart TYPE SORTED TABLE OF char4 WITH UNIQUE KEY table_line.

* Get unique wage types
  LOOP AT gt_recon INTO gs_recon.
    INSERT gs_recon-lgart INTO TABLE lt_lgart.
  ENDLOOP.

* Build summary per wage type
  LOOP AT lt_lgart INTO DATA(lv_lgart).
    CLEAR gs_summary.
    gs_summary-lgart = lv_lgart.

    LOOP AT gt_recon INTO gs_recon WHERE lgart = lv_lgart.
      gs_summary-prev_total = gs_summary-prev_total + gs_recon-prev_amount.
      gs_summary-curr_total = gs_summary-curr_total + gs_recon-curr_amount.
    ENDLOOP.

    gs_summary-difference = gs_summary-curr_total - gs_summary-prev_total.
    APPEND gs_summary TO gt_summary.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_REPORT
*&---------------------------------------------------------------------*
FORM display_report.

  DATA: lo_alv TYPE REF TO cl_salv_table.

  WRITE: / 'HCM Reconciliation Report'.
  WRITE: / 'Previous Period:', gv_prev_per, 'Current Period:', gv_curr_per.
  ULINE.

* Display Summary
  WRITE: / 'SUMMARY BY WAGE TYPE'.
  ULINE.
  WRITE: / 'WType', 10 'Previous', 25 'Current', 40 'Difference'.
  ULINE.

  LOOP AT gt_summary INTO gs_summary.
    WRITE: / gs_summary-lgart,
             10 gs_summary-prev_total,
             25 gs_summary-curr_total,
             40 gs_summary-difference.
  ENDLOOP.

  ULINE.
  SKIP.

* Display Details using ALV
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_recon ).

      lo_alv->display( ).

    CATCH cx_salv_msg.
      PERFORM display_details.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DETAILS
*&---------------------------------------------------------------------*
FORM display_details.

  WRITE: / 'DETAIL BY EMPLOYEE'.
  ULINE.
  WRITE: / 'Pernr', 12 'Name', 35 'WType', 42 'Previous', 55 'Current', 68 'Diff'.
  ULINE.

  LOOP AT gt_recon INTO gs_recon.
    WRITE: / gs_recon-pernr,
             12 gs_recon-ename,
             35 gs_recon-lgart,
             42 gs_recon-prev_amount,
             55 gs_recon-curr_amount,
             68 gs_recon-difference.
  ENDLOOP.

ENDFORM.
