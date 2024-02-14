#include <stdio.h>
#include <windows.h>
#include <string.h>
#include "feasatools.h"

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Dynamic Library
********************************************************/

tFeasa_Parse_Int16 Feasa_Parse_Int16;
tFeasa_Parse_Int32 Feasa_Parse_Int32;
tFeasa_Parse_Float Feasa_Parse_Float;

tFeasa_Parse_RGBI Feasa_Parse_RGBI;
tFeasa_Parse_RGBI_All Feasa_Parse_RGBI_All;
tFeasa_Parse_HSI Feasa_Parse_HSI;
tFeasa_Parse_HSI_All Feasa_Parse_HSI_All;
tFeasa_Parse_xy Feasa_Parse_xy;
tFeasa_Parse_xy_All Feasa_Parse_xy_All;
tFeasa_Parse_uv Feasa_Parse_uv;
tFeasa_Parse_uv_All Feasa_Parse_uv_All;
tFeasa_Parse_CCT Feasa_Parse_CCT;
tFeasa_Parse_CCT_All Feasa_Parse_CCT_All;
tFeasa_Parse_Wavelength Feasa_Parse_Wavelength;
tFeasa_Parse_Wavelength_All Feasa_Parse_Wavelength_All;
tFeasa_Parse_WI Feasa_Parse_WI;
tFeasa_Parse_WI_All Feasa_Parse_WI_All;
tFeasa_Parse_Intensity Feasa_Parse_Intensity;
tFeasa_Parse_Intensity_All Feasa_Parse_Intensity_All;

tFeasa_Parse_Spectrum Feasa_Parse_Spectrum;

HINSTANCE hFeasaToolsDLL;

int FeasaTools_Load(int is64bit, char * PathToDLL)
{
	int l = strlen(PathToDLL);

	char * DLLPath = (char *)malloc(sizeof(char) * (l + 100));
	DLLPath[0] = '\0';

	if (l > 0) {
		strcpy(DLLPath, PathToDLL);
		if ((DLLPath[l - 1] != '/') && (DLLPath[l - 1] != '\\'))
			strcat(DLLPath, "\\");
	}

	if ((strstr(DLLPath, ".dll") == NULL) && (strstr(DLLPath, ".dll") == NULL)) {
		if (is64bit)
			strcat(DLLPath, "feasa_tools64.dll");
		else
			strcat(DLLPath, "feasa_tools.dll");
	}

	hFeasaToolsDLL = LoadLibrary(DLLPath);
	if (hFeasaToolsDLL == NULL) {
		free(DLLPath);
		return 0;
	}

	// Get Addresses of DLL functions
	// Declare pointers to call functions
	Feasa_Parse_Int16 = (tFeasa_Parse_Int16)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_Int16");
	Feasa_Parse_Int32 = (tFeasa_Parse_Int32)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_Int32");
	Feasa_Parse_Float = (tFeasa_Parse_Float)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_Float");
	if ((Feasa_Parse_Int16 == NULL) || (Feasa_Parse_Int32 == NULL) || (Feasa_Parse_Float == NULL)) {
		FreeLibrary((HMODULE)hFeasaToolsDLL);
		free(DLLPath);
		return 0;
	}

	Feasa_Parse_RGBI = (tFeasa_Parse_RGBI)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_RGBI");
	Feasa_Parse_RGBI_All = (tFeasa_Parse_RGBI_All)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_RGBI_All");
	Feasa_Parse_HSI = (tFeasa_Parse_HSI)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_HSI");
	Feasa_Parse_HSI_All = (tFeasa_Parse_HSI_All)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_HSI_All");
	Feasa_Parse_xy = (tFeasa_Parse_xy)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_xy");
	Feasa_Parse_xy_All = (tFeasa_Parse_xy_All)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_xy_All");
	Feasa_Parse_uv = (tFeasa_Parse_uv)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_uv");
	Feasa_Parse_uv_All = (tFeasa_Parse_uv_All)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_uv_All");
	Feasa_Parse_CCT = (tFeasa_Parse_CCT)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_CCT");
	Feasa_Parse_CCT_All = (tFeasa_Parse_CCT_All)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_CCT_All");
	Feasa_Parse_Wavelength = (tFeasa_Parse_Wavelength)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_Wavelength");
	Feasa_Parse_Wavelength_All = (tFeasa_Parse_Wavelength_All)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_Wavelength_All");
	Feasa_Parse_WI = (tFeasa_Parse_WI)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_WI");
	Feasa_Parse_WI_All = (tFeasa_Parse_WI_All)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_WI_All");
	Feasa_Parse_Intensity = (tFeasa_Parse_Intensity)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_Intensity");
	Feasa_Parse_Intensity_All = (tFeasa_Parse_Intensity_All)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_Intensity_All");
	if ((Feasa_Parse_RGBI == NULL) || (Feasa_Parse_RGBI_All == NULL) || (Feasa_Parse_HSI == NULL) || (Feasa_Parse_HSI_All == NULL) || (Feasa_Parse_xy == NULL) || (Feasa_Parse_xy_All == NULL) || (Feasa_Parse_uv == NULL) || (Feasa_Parse_uv_All == NULL) || (Feasa_Parse_CCT == NULL) || (Feasa_Parse_CCT_All == NULL) || (Feasa_Parse_Wavelength == NULL) || (Feasa_Parse_Wavelength_All == NULL) || (Feasa_Parse_WI == NULL) || (Feasa_Parse_WI_All == NULL) || (Feasa_Parse_Intensity == NULL) || (Feasa_Parse_Intensity_All == NULL)) {
		FreeLibrary((HMODULE)hFeasaToolsDLL);
		free(DLLPath);
		return 0;
	}

	Feasa_Parse_Spectrum = (tFeasa_Parse_Spectrum)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_Spectrum");
	if ((Feasa_Parse_Spectrum == NULL) ) {
		FreeLibrary((HMODULE)hFeasaToolsDLL);
		free(DLLPath);
		return 0;
	}

	free(DLLPath);

	return 1;
}

void FeasaTools_UnLoad()
{
	FreeLibrary((HMODULE)hFeasaToolsDLL);
}

void FormatDecimal(char * buffer)
{
	int i,l;
	float f=0;
	char decChr, c;

	l= strlen(buffer);

	if ( l>0 ) {
		sscanf("0.253","%f", &f);

		if ( f==(float)(0.253) ) {
			decChr='.'; c = ',';
		} else {
			decChr=','; c = '.';
		}

		for (i=0; i<l; i++) {
			if (buffer[i]==c) buffer[i] = decChr;
		}
	}
}
