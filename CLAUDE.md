# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Overview

This repository contains ABAP code for NADEC SAP development. All files in this repository are ABAP programs and related SAP artifacts.

## File Types

- `*.abap` - ABAP source code files
- `*.asddls` - CDS view definitions
- `*.acinf` - ABAP class includes
- `*.fugr` - Function groups
- `*.tabl` - Table definitions

## ABAP Coding Standards

### Naming Conventions
- Reports: `Z_*` or `Y_*` prefix
- Classes: `ZCL_*` or `YCL_*` prefix
- Interfaces: `ZIF_*` or `YIF_*` prefix
- Function Modules: `Z_*` or `Y_*` prefix
- Variables: `lv_` (local), `gv_` (global), `iv_` (importing), `ev_` (exporting), `rv_` (returning)
- Tables: `lt_` (local), `gt_` (global), `it_` (importing), `et_` (exporting), `rt_` (returning)
- Structures: `ls_` (local), `gs_` (global)

### Code Structure
- Always include program header comments
- Use meaningful variable names
- Add comments for complex logic
- Follow SAP ABAP best practices

### Common ABAP Statements
- `DATA` - Variable declarations
- `WRITE` - Output statements
- `SELECT` - Database queries
- `LOOP AT` - Internal table iteration
- `IF/ELSE/ENDIF` - Conditional logic
- `CASE/WHEN/ENDCASE` - Switch statements
- `DO/ENDDO` - Counted loops
- `WHILE/ENDWHILE` - Conditional loops

## Development Guidelines

- All code should be syntactically correct ABAP
- Use proper indentation (2 spaces)
- Handle exceptions appropriately
- Optimize database access with SELECT statements
- Use internal tables efficiently
