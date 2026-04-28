//!STARTOFREGISTRYGENERATEDFILE 'ExtPtfmLoadsDX_Types.h'
//!
//! WARNING This file is generated automatically by the FAST registry.
//! Do not edit.  Your changes to this file will be lost.
//!

#ifndef _ExtPtfmLoadsDX_TYPES_H
#define _ExtPtfmLoadsDX_TYPES_H

#ifdef _WIN32 //define something for Windows (32-bit)
	#include "stdbool.h"
	#define CALL __declspec(dllexport)
#elif _WIN64 //define something for Windows (64-bit)
	#include "stdbool.h"
	#define CALL __declspec(dllexport) 
#else
	#include <stdbool.h>
	#define CALL 
#endif

typedef struct ExtPtfmLdDX_InputType {
	void *object;
	double *ptfmDef;            int ptfmDef_Len;
} ExtPtfmLdDX_InputType_t;

typedef struct ExtPtfmLdDX_ParameterType {
	void *object;
	double *ptfmRefPos;         int ptfmRefPos_Len;
} ExtPtfmLdDX_ParameterType_t;

typedef struct ExtPtfmLdDX_OutputType {
	void *object;
	double *ptfmLd;             int ptfmLd_Len;
	double *ptfmAddedMass;      int ptfmAddedMass_Len;
} ExtPtfmLdDX_OutputType_t;

typedef struct ExtPtfmLdDX_UserData {
	ExtPtfmLdDX_InputType_t      ExtPtfmLdDX_Input;
	ExtPtfmLdDX_ParameterType_t  ExtPtfmLdDX_Param;
	ExtPtfmLdDX_OutputType_t     ExtPtfmLdDX_Output;
} ExtPtfmLdDX_t;

#endif // _ExtPtfmLoadsDX_TYPES_H

//!ENDOFREGISTRYGENERATEDFILE
