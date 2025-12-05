*&---------------------------------------------------------------------*
*& Report ZLOAN_BALANCE_REPORT
*& Transaction: ZLBREP
*& Description: Loan Balance Report
*& Feedback: Does not exist on PRD
*& Owner: Mehboob
*&---------------------------------------------------------------------*
REPORT zloan_balance_report.

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: pa0001,  " HR Master Record: Infotype 0001 (Org. Assignment)
        pa0045.  " HR Master Record: Infotype 0045 (Loans)

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_loan,
         pernr      TYPE pa0001-pernr,      " Personnel Number
         ename      TYPE pa0001-ename,      " Employee Name
         bukrs      TYPE pa0001-bukrs,      " Company Code
         werks      TYPE pa0001-werks,      " Personnel Area
         daression  TYPE pa0045-daression,  " Loan Type
         begda      TYPE pa0045-begda,      " Start Date
         endda      TYPE pa0045-endda,      " End Date
         loan_amount TYPE p DECIMALS 2,     " Original Loan Amount
         paid_amount TYPE p DECIMALS 2,     " Paid Amount
         balance    TYPE p DECIMALS 2,      " Outstanding Balance
       END OF ty_loan.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_loans    TYPE TABLE OF ty_loan,
      gs_loan     TYPE ty_loan,
      gv_total_balance TYPE p DECIMALS 2.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_pernr FOR pa0001-pernr,      " Personnel Number
                  s_bukrs FOR pa0001-bukrs,      " Company Code
                  s_werks FOR pa0001-werks.      " Personnel Area
  PARAMETERS:     p_date  TYPE sy-datum DEFAULT sy-datum.  " Key Date
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_lntyp FOR pa0045-daression.  " Loan Type
  PARAMETERS:     p_outst TYPE c AS CHECKBOX DEFAULT 'X'.  " Outstanding Only
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'Selection Criteria'.
  TEXT-002 = 'Loan Filters'.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_loans.
  PERFORM calculate_balances.
  PERFORM display_report.

*&---------------------------------------------------------------------*
*& Form SELECT_LOANS
*&---------------------------------------------------------------------*
FORM select_loans.

  SELECT a~pernr a~ename a~bukrs a~werks
         b~daressionn b~begda b~endda
    INTO CORRESPONDING FIELDS OF TABLE gt_loans
    FROM pa0001 AS a
    INNER JOIN pa0045 AS b ON a~pernr = b~pernr
    WHERE a~pernr IN s_pernr
      AND a~bukrs IN s_bukrs
      AND a~werks IN s_werks
      AND b~daressionn IN s_lntyp
      AND p_date BETWEEN a~begda AND a~endda
      AND p_date BETWEEN b~begda AND b~endda.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALCULATE_BALANCES
*&---------------------------------------------------------------------*
FORM calculate_balances.

  LOOP AT gt_loans INTO gs_loan.
*   Calculate outstanding balance
*   This is placeholder logic - actual calculation from payroll clusters
    gs_loan-balance = gs_loan-loan_amount - gs_loan-paid_amount.

    MODIFY gt_loans FROM gs_loan.
    gv_total_balance = gv_total_balance + gs_loan-balance.
  ENDLOOP.

* Filter outstanding only
  IF p_outst = 'X'.
    DELETE gt_loans WHERE balance <= 0.
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
          t_table      = gt_loans ).

      lo_alv->display( ).

    CATCH cx_salv_msg.
      PERFORM display_simple.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_SIMPLE
*&---------------------------------------------------------------------*
FORM display_simple.

  WRITE: / 'Loan Balance Report'.
  WRITE: / 'Date:', p_date.
  ULINE.

  WRITE: / 'Pernr', 15 'Name', 40 'Loan Type', 55 'Balance'.
  ULINE.

  LOOP AT gt_loans INTO gs_loan.
    WRITE: / gs_loan-pernr,
             15 gs_loan-ename,
             40 gs_loan-daressionn,
             55 gs_loan-balance.
  ENDLOOP.

  ULINE.
  WRITE: / 'Total Outstanding:', 55 gv_total_balance.

ENDFORM.
