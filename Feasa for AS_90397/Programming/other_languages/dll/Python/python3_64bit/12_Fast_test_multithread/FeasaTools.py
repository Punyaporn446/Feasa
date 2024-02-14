import ctypes

#Note: feasa_tools.dll file has to be used for 32bit targets
FeasaTools = ctypes.WinDLL('feasa_tools64.dll')

Parse_Int16 = FeasaTools['Feasa_Parse_Int16']
Parse_Int16.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_char)]
Parse_Int16.restype = ctypes.c_int16

Parse_Int32 = FeasaTools['Feasa_Parse_Int32']
Parse_Int32.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_char)]
Parse_Int32.restype = ctypes.c_int32

Parse_Float = FeasaTools['Feasa_Parse_Float']
Parse_Float.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_char)]
Parse_Float.restype = ctypes.c_float

Parse_RGBI = FeasaTools['Feasa_Parse_RGBI']
Parse_RGBI.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_int)]
Parse_RGBI.restype = ctypes.c_int

Parse_RGBI_All = FeasaTools['Feasa_Parse_RGBI_All']
Parse_RGBI_All.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_int)]
Parse_RGBI_All.restype = ctypes.c_int

Parse_HSI = FeasaTools['Feasa_Parse_HSI']
Parse_HSI.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Parse_HSI.restype = ctypes.c_int

Parse_HSI_All = FeasaTools['Feasa_Parse_HSI_All']
Parse_HSI_All.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Parse_HSI_All.restype = ctypes.c_int

Parse_xy = FeasaTools['Feasa_Parse_xy']
Parse_xy.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float)]
Parse_xy.restype = ctypes.c_int

Parse_xy_All = FeasaTools['Feasa_Parse_xy_All']
Parse_xy_All.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float)]
Parse_xy_All.restype = ctypes.c_int

Parse_uv = FeasaTools['Feasa_Parse_uv']
Parse_uv.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float)]
Parse_uv.restype = ctypes.c_int

Parse_uv_All = FeasaTools['Feasa_Parse_uv_All']
Parse_uv_All.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float)]
Parse_uv_All.restype = ctypes.c_int

Parse_CCT = FeasaTools['Feasa_Parse_CCT']
Parse_CCT.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_float)]
Parse_CCT.restype = ctypes.c_int

Parse_CCT_All = FeasaTools['Feasa_Parse_CCT_All']
Parse_CCT_All.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_float)]
Parse_CCT_All.restype = ctypes.c_int

Parse_Wavelength = FeasaTools['Feasa_Parse_Wavelength']
Parse_Wavelength.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float)]
Parse_Wavelength.restype = ctypes.c_int

Parse_Wavelength_All = FeasaTools['Feasa_Parse_Wavelength_All']
Parse_Wavelength_All.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float)]
Parse_Wavelength_All.restype = ctypes.c_int

Parse_WI = FeasaTools['Feasa_Parse_WI']
Parse_WI.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int)]
Parse_WI.restype = ctypes.c_int

Parse_WI_All = FeasaTools['Feasa_Parse_WI_All']
Parse_WI_All.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int)]
Parse_WI_All.restype = ctypes.c_int

Parse_Intensity = FeasaTools['Feasa_Parse_Intensity']
Parse_Intensity.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_int)]
Parse_Intensity.restype = ctypes.c_int

Parse_Intensity_All = FeasaTools['Feasa_Parse_Intensity_All']
Parse_Intensity_All.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_int)]
Parse_Intensity_All.restype = ctypes.c_int

Parse_Spectrum = FeasaTools['Feasa_Parse_Spectrum']
Parse_Spectrum.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_double)]
Parse_Spectrum.restype = ctypes.c_int

def FormatDecimal(Number):
	DecimalFormatDot = False

	try:
		nn = float("3.21")
	except:
		return Number.replace('.', ',')

	try:
		if nn==3.21:
			DecimalFormatDot = False
		else:
			DecimalFormatDot = False
	except:
		return Number.replace('.', ',')
		
	if DecimalFormatDot:
		return Number.replace(',', '.')
	else:
		return Number.replace('.', ',')
