using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;

namespace FeasaLib
{
    class FeasaCom
    {
        private const string LIB_PATH = "libfeasacom.x86_64.so";

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetLibraryVersion")]
        public static extern void GetLibraryVersion(StringBuilder Version);

        // Basic Comm functions

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Open")]
        public static extern int Open(string DevPath, int Baudrate);
    
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Send")]
        public static extern int Send(string DevPath, string Command, StringBuilder ResponseText);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Close")]
        public static extern int Close(string DevPath);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_OpenSN")]
        public static extern int OpenSN(string SerialNumber, int Baudrate);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_SendSN")]
        public static extern int SendSN(string SerialNumber, string Command, StringBuilder ResponseText);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_CloseSN")]
        public static extern int CloseSN(string SerialNumber);


        // Comm helper functions
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_SendToAll")]
        public static extern int SendToAll(int[] ReturnValues, string Command, IntPtr[] pResponses);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_SendToAll_NR")]
        public static extern int SendToAll_NR(int[] ReturnValues, string Command);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Open_Multi")]
        public static extern int Open_Multi(int[] ReturnValues, [In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] DevPaths, int nPorts, int Baudrate);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Close_Multi")]
        public static extern int Close_Multi(int[] ReturnValues, [In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] DevPaths, int nPorts);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Send_Multi")]
        public static extern int Send_Multi(int[] ReturnValues, [In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] DevPaths, int nPorts, [In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] Commands, IntPtr[] pResponses);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Send_Multi_NR")]
        public static extern int Send_Multi_NR(int[] ReturnValues, string DevPaths, int nPorts, string Commands, char CommandSeparator);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_OpenSN_Multi")]
        public static extern int OpenSN_Multi(int[] ReturnValues, [In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] SerialNumbers, int nSerials, int Baudrate);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_CloseSN_Multi")]
        public static extern int CloseSN_Multi(int[] ReturnValues, [In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] SerialNumbers, int nSerials);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_SendSN_Multi")]
        public static extern int SendSN_Multi(int[] ReturnValues, [In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] SerialNumbers, int nSerials, [In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] Commands, IntPtr[] pResponses);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_CloseAll")]
        public static extern int CloseAll();

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetResponseByPort")]
        public static extern int GetResponseByPort(string DevPath, StringBuilder ResponseText);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetOpenedPorts")]
        public static extern int GetOpenedPorts(IntPtr[] DevPaths);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetOpenedPortsS")]
        public static extern int GetOpenedPortsS(StringBuilder DevPaths, Char Delimiter);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_OpenProject")]
        public static extern int OpenProject(string Path);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_CloseProject")]
        public static extern int CloseProject();

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_SendByID")]
        public static extern int SendByID(string DeviceID, string Command, StringBuilder ResponseText);
        
        
        // Test functions
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Capture")]
        public static extern int Capture(string DevPath, int isPWM, int CaptureRange, int CapturePWMFrames);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_CaptureFromAll")]
        public static extern int CaptureFromAll(int[] ReturnValues, int isPWM, int CaptureRange, int CapturePWMFrames);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_SpectrometerCapture")]
        public static extern int SpectrometerCapture(string DevPath, int isPWM, int UseCustomExposure, float ExposureTime);
        
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_SpectrometerDark")]
        public static extern int SpectrometerDark(string DevPath, int isPWM, int UseCustomExposure, float ExposureTime);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_CaptureFromAllSpectrometers")]
        public static extern int CaptureFromAllSpectrometers(int[] ReturnValues, int isPWM, int UseCustomExposure, float ExposureTime);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_Setup")]
        public static extern int Sequence_Setup(string DevPath, int StartDelay, int CaptureTime, int TimeBetweenCaptures, int SampleCount, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_Capture")]
        public static extern int Sequence_Capture(string DevPath, int Fiber);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_ReadIntensity")]
        public static extern int Sequence_ReadIntensity(string DevPath, int Fiber, int[] IntensityValues);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_ReadxyI")]
        public static extern int Sequence_ReadxyI(string DevPath, int Fiber, float[] xValues, float[] yValues, int[] IntensityValues);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_ReadHSI")]
        public static extern int Sequence_ReadHSI(string DevPath, int Fiber, float[] HueValues, int[] SaturationValues, int[] IntensityValues);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_ReadRGBI")]
        public static extern int Sequence_ReadRGBI(string DevPath, int Fiber, byte[] RedValues, byte[] GreenValues, byte[] BlueValues, int[] IntensityValues);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_ReadCCT")]
        public static extern int Sequence_ReadCCT(string DevPath, int Fiber, int[] CCTValues, float[] deltauvValues);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_ReadWavelength")]
        public static extern int Sequence_ReadWavelength(string DevPath, int Fiber, int[] WavelengthValues);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_GetPattern")]
        public static extern int Sequence_GetPattern(string DevPath, int[] IntensityValues, ref int StatusCount, int[] PatternTimes, int[] PatternIntensities);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_GetSweepingPattern")]
        public static extern int Sequence_GetSweepingPattern(string DevPath, int LEDCount, int isOffToOn, int[] LowTimes, int[] HighTimes, int[] IntensityValues);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_GetFrequency")]
        public static extern int Sequence_GetFrequency(string DevPath, int[] IntensityValues, ref float Frequency, ref float DC, ref int CycleCount);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_FindTestSettings")]
        public static extern int Sequence_FindTestSettings(string DevPath, int TotalLEDCount, int FiberToTest, int SignalSpeed, int BlinkingSpeed, int MinCycleCount, int TimeResolutionIsImportant, ref int CaptureTime, ref int WaitTime, ref int SampleCount);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_SetPatternThresholdHigh")]
        public static extern int Sequence_SetPatternThresholdHigh(string DevPath, int Intensity);
        
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Sequence_SetPatternThresholdLow")]
        public static extern int Sequence_SetPatternThresholdLow(string DevPath, int Intensity);


        // Daisy-chain functions
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_DaisyChain_Add")]
        public static extern int DaisyChain_Add(string DevPath, string SerialNumber);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_DaisyChain_Del")]
        public static extern int DaisyChain_Del(string DevPath, string SerialNumber);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_DaisyChain_Clear")]
        public static extern int DaisyChain_Clear(string DevPath);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_DaisyChain_Send")]
        public static extern int DaisyChain_Send(string DevPath, string SerialNumber, string Command, StringBuilder ResponseText);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_DaisyChain_Capture")]
        public static extern int DaisyChain_Capture(string DevPath, int isPWM, int CaptureRange, int CapturePWMFrames);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_DaisyChain_SpectrometerCapture")]
        public static extern int DaisyChain_SpectrometerCapture(string DevPath, int UsePresetExposure, int UseCustomExposure, float CapturePWMFrames);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_DaisyChain_SpectrometerDark")]
        public static extern int DaisyChain_SpectrometerDark(string DevPath, int isPWM, int UsePresetExposure, float ExposureTime);


        // External Trigger functions
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_ExternalTrigger_Listen")]
        public static extern int ExternalTrigger_Listen(string DevPath);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_ExternalTrigger_Abort")]
        public static extern int ExternalTrigger_Abort(string DevPath);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_ExternalTrigger_isFinished")]
        public static extern int ExternalTrigger_isFinished(string DevPath);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_ExternalTrigger_Enable")]
        public static extern int ExternalTrigger_Enable(string DevPath, int CaptureRange, int isPWM, string OutputType, int PreDelay, int PostDelay, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_ExternalTrigger_Disable")]
        public static extern int ExternalTrigger_Disable(string DevPath, int toFlash);


        // Comm handling functions
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_EnumPorts")]
        public static extern int EnumPorts();

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_IsConnected")]
        public static extern int IsConnected(StringBuilder DevPath, string SerialNumber, int Baudrate);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_AreConnected")]
        public static extern int AreConnected(IntPtr[] pDevPaths, [In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] SerialNumbers, int nSerials, int Baudrate);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_AreConnectedS")]
        public static extern int AreConnectedS(StringBuilder DevPaths, string SerialNumbers, int Baudrate);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Detect")]
        public static extern int Detect(IntPtr[] pDevPaths, int Baudrate);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_DetectS")]
        public static extern int DetectS(StringBuilder DevPaths, char Delimiter, int Baudrate);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_DetectSN")]
        public static extern int DetectSN(IntPtr[] pSerialNumbers, int Baudrate);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_AddDetectionFilter")]
        public static extern void AddDetectionFilter(string Filter);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_ClearDetectionFilters")]
        public static extern void ClearDetectionFilters();

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_IsPortAvailable")]
        public static extern int IsPortAvailable(string DevPath);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_ListPortsDetected")]
        public static extern int ListPortsDetected(IntPtr[] ListOfPortsDetected);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_ListPortsDetectedTxt")]
        public static extern int ListPortsDetectedTxt(StringBuilder ListOfPortsDetected, string Delimiter);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_SetResponseTimeout")]
        public static extern int SetResponseTimeout(uint Timeout);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_SetResponseTimeoutAuto")]
        public static extern int SetResponseTimeoutAuto(string DevPath, int Status);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetBaudrate")]
        public static extern long GetBaudrate(string DevPath);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetDeviceType")]
        public static extern int GetDeviceType(string DevPath, StringBuilder DeviceType);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetError_Description")]
        public static extern void GetError_Description(StringBuilder ErrorDescription);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetError_DescriptionByPort")]
        public static extern void GetError_DescriptionByPort(string DevPath, StringBuilder ErrorDescription);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetError_DescriptionBySN")]
        public static extern void GetError_DescriptionBySN(string SerialNumber, StringBuilder ErrorDescription);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetPortBySN")]
        public static extern int GetPortBySN(StringBuilder DevPath, string SerialNumber);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetSNByPort")]
        public static extern int GetSNByPort(StringBuilder SerialNumber, string DevPath);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_GetPortByID")]
        public static extern int GetPortByID(string DeviceID, StringBuilder DevPath);


        // Binning
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_Binning_GetBinFromVECFile")]
        public static extern int Binning_GetBinFromVECFile(string Path, float x, float y, StringBuilder ResultBinName);


        // UserCal functions
        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_ResetIntensity")]
        public static extern int UserCal_ResetIntensity(string DevPath, int Fiber, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_GetIntensityGain")]
        public static extern int UserCal_GetIntensityGain(string DevPath, int Fiber, ref int Gain);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_SetIntensityGain")]
        public static extern int UserCal_SetIntensityGain(string DevPath, int Fiber, int Gain, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_AdjustIntensity")]
        public static extern int UserCal_AdjustIntensity(string DevPath, int Fiber, int IntensityRef, int isPWM, int CaptureRange, int toFlash);


        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_ResetxyOffsets")]
        public static extern int UserCal_ResetxyOffsets(string DevPath, int Fiber, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_GetxyOffsets")]
        public static extern int UserCal_GetxyOffsets(string DevPath, int Fiber, ref float xOffset, ref float yOffset);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_SetxyOffsets")]
        public static extern int UserCal_SetxyOffsets(string DevPath, int Fiber, float xOffset, float yOffset, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_AdjustxyOffsets")]
        public static extern int UserCal_AdjustxyOffsets(string DevPath, int Fiber, float xRef, float yRef, int ToFlash);


        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_ResetWavelengthOffset")]
        public static extern int UserCal_ResetWavelengthOffset(string DevPath, int Fiber, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_GetWavelengthOffset")]
        public static extern int UserCal_GetWavelengthOffset(string DevPath, int Fiber, ref int WavelengthOffset);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_SetWavelengthOffset")]
        public static extern int UserCal_SetWavelengthOffset(string DevPath, int Fiber, int WavelengthOffset, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_AdjustWavelengthOffset")]
        public static extern int UserCal_AdjustWavelengthOffset(string DevPath, int Fiber, int WavelengthRef, int toFlash);


        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_ResetAbsInt")]
        public static extern int UserCal_ResetAbsInt(string DevPath, int Fiber, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_GetAbsIntFactor")]
        public static extern int UserCal_GetAbsIntFactor(string DevPath, int Fiber, ref double Factor);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_SetAbsIntFactor")]
        public static extern int UserCal_SetAbsIntFactor(string DevPath, int Fiber, double Factor, int toFlash);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_AdjustAbsInt")]
        public static extern int UserCal_AdjustAbsInt(string DevPath, int Fiber, double AbsIntRef, int toFlash);


        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_ResetRGBAdj")]
        public static extern int UserCal_ResetRGBAdj(string DevPath, int Fiber);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_TakeRGBCurrentValues")]
        public static extern int UserCal_TakeRGBCurrentValues(string DevPath, int Fiber, char Color);

        [DllImport(LIB_PATH, EntryPoint = "FeasaCom_UserCal_AdjustRGB")]
        public static extern int UserCal_AdjustRGB(string DevPath, int Fiber, float xRefRed, float yRefRed, double AbsIntRefRed, float xRefGreen, float yRefGreen, double AbsIntRefGreen, float xRefBlue, float yRefBlue, double AbsIntRefBlue);
    }

    static class FeasaTools
    {
        public static void InitializeArrayOfStrings(ref string[] mArray, int StringSize)
        {
            for (int i = 0; i < mArray.Length; i++)
                mArray[i] = new string('\0', StringSize);
        }


        public static IntPtr[] InitializePtrArray(int MaxElements, int MaxStringSize)
        {
            IntPtr[] pArray = new IntPtr[MaxElements];
            for (int i = 0; i < pArray.Length; i++)
                pArray[i] = Marshal.AllocCoTaskMem(MaxStringSize);
            return pArray;
        }

        public static string[] PtrArrayToStringArray(IntPtr[] pArray, int Count)
        {
            List<string> lstArray = new List<string>();
            try
            {
                for (int i = 0; i < Count; i++)
                {
                    StringBuilder sb = new StringBuilder(Marshal.PtrToStringAnsi(pArray[i]));
                    lstArray.Add(sb.ToString());
                }
            }
            catch { }
            return lstArray.ToArray();
        }

        public static void FreePtrArray(IntPtr[] pArray)
        {
            for (int i = 0; i < pArray.Length; i++)
                Marshal.FreeCoTaskMem(pArray[i]);
        }
    }
}
