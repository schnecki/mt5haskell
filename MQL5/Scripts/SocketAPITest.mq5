//+------------------------------------------------------------------+
//|                                              SocketAPITest.mq5   |
//|                                    Test Socket API availability |
//+------------------------------------------------------------------+
#property copyright "Test"
#property version   "1.00"
#property strict

//+------------------------------------------------------------------+
//| Script program start function                                    |
//+------------------------------------------------------------------+
void OnStart()
{
   Print("Testing Socket API availability...");
   Print("MT5 Build: ", (string)TerminalInfoInteger(TERMINAL_BUILD));
   
   //--- Test SocketCreate
   int socket = SocketCreate();
   if(socket == INVALID_HANDLE)
   {
      Print("SocketCreate FAILED. Error: ", GetLastError());
      Print("Socket API may not be available or disabled in settings.");
   }
   else
   {
      Print("SocketCreate SUCCESS! Socket handle: ", socket);
      SocketClose(socket);
   }
}
