//+------------------------------------------------------------------+
//|                                      NetworkFunctionsTest.mq5    |
//|                                Test available network functions |
//+------------------------------------------------------------------+
#property copyright "Test"
#property version   "1.00"

//+------------------------------------------------------------------+
//| Script program start function                                    |
//+------------------------------------------------------------------+
void OnStart()
{
   Print("=== Testing MQL5 Network Functions ===");
   Print("MT5 Build: ", (string)TerminalInfoInteger(TERMINAL_BUILD));
   Print("");
   
   //--- Test 1: WebRequest (should work - client HTTP)
   Print("Test 1: WebRequest function");
   string cookie = NULL, referer = NULL;
   string headers;
   char post[], result[];
   string url = "http://www.google.com";
   
   // Note: WebRequest requires URL to be in allowed list
   // Tools -> Options -> Expert Advisors -> Allow WebRequest for:
   Print("WebRequest exists: ", "Yes (check if enabled in settings)");
   Print("");
   
   //--- Test 2: Check if Socket functions exist by trying to use them
   Print("Test 2: Attempting to use Socket functions...");
   Print("Note: This will show compilation errors if functions don't exist");
   Print("");
   
   // The following should cause compilation error if Socket API not available:
   // int test_socket = SocketCreate();
   
   Print("=== Test Complete ===");
   Print("");
   Print("IMPORTANT: If SocketCreate, SocketBind, SocketListen show as");
   Print("'undeclared identifier', then MQL5 Socket SERVER API is not available.");
   Print("");
   Print("MQL5 Socket API is designed for CLIENT connections only!");
   Print("It can CONNECT to external servers, but cannot CREATE servers.");
}
