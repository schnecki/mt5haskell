//+------------------------------------------------------------------+
//|                                           MT5RestAPIBridge.mq5   |
//|                        Copyright 2025, Manuel Schneckenreither   |
//|                                                                  |
//+------------------------------------------------------------------+
#property copyright "Copyright 2025, Manuel Schneckenreither"
#property link      "https://github.com/schnecki/mt5haskell"
#property version   "1.00"
#property description "File-based REST API Bridge for MT5 Trading Operations"
#property description "Reads JSON requests from file, executes trades, writes responses"

//--- Include necessary libraries
#include <Trade\Trade.mqh>
#include <Trade\PositionInfo.mqh>
#include <Trade\OrderInfo.mqh>
#include <JAson.mqh>  // JSON library for MQL5 (should be in Include directory)
#include <Files\FileTxt.mqh>

//--- Configuration
input string RequestFile = "mt5_api_request.json";   // Request file name
input string ResponseFile = "mt5_api_response.json"; // Response file name
input int CheckIntervalInSecs = 1;                   // Check interval in seconds (minimum 1)
input bool EnableLogging = true;                     // Enable detailed logging

//--- Global variables
CTrade trade;
CPositionInfo positionInfo;
COrderInfo orderInfo;
CFileTxt requestFileHandle;
CFileTxt responseFileHandle;
datetime lastProcessedTime = 0;

//+------------------------------------------------------------------+
//| Expert initialization function                                     |
//+------------------------------------------------------------------+
int OnInit()
{
   Print("MT5 REST API Bridge initializing...");

   //--- Initialize trading object
   trade.SetExpertMagicNumber(0);
   trade.SetDeviationInPoints(20);
   trade.SetTypeFilling(ORDER_FILLING_FOK);
   trade.SetAsyncMode(false);

   //--- Set up timer for periodic file checking
   // EventSetTimer expects seconds (minimum 1 second)
   int timerSeconds = MathMax(1, CheckIntervalInSecs);
   if(!EventSetTimer(timerSeconds))
   {
      Print("ERROR: Failed to set timer. File checking may not work properly.");
      return(INIT_FAILED);
   }

   Print("MT5 REST API Bridge started successfully");
   Print("Checking for requests every ", timerSeconds, " second(s)");
   Print("Waiting for requests in: ", TerminalInfoString(TERMINAL_COMMONDATA_PATH), "\\Files\\", RequestFile);
   Print("Responses will be written to: ", TerminalInfoString(TERMINAL_COMMONDATA_PATH), "\\Files\\", ResponseFile);

   return(INIT_SUCCEEDED);
}

//+------------------------------------------------------------------+
//| Expert deinitialization function                                   |
//+------------------------------------------------------------------+
void OnDeinit(const int reason)
{
   //--- Kill timer
   EventKillTimer();
   
   Print("MT5 REST API Bridge shutting down. Reason: ", reason);
}

//+------------------------------------------------------------------+
//| Expert timer function (called every CheckIntervalMs)             |
//+------------------------------------------------------------------+
void OnTimer()
{
   //--- Process any pending file requests
   ProcessFileRequests();
}

//+------------------------------------------------------------------+
//| Expert tick function (also check on price ticks for lower latency)|
//+------------------------------------------------------------------+
void OnTick()
{
   //--- Also process requests on ticks for lower latency
   ProcessFileRequests();
}

//+------------------------------------------------------------------+
//| Process file-based requests                                        |
//+------------------------------------------------------------------+
void ProcessFileRequests()
{
   //--- Check if request file exists and has been modified
   if(!requestFileHandle.Open(RequestFile, FILE_READ|FILE_TXT|FILE_ANSI|FILE_COMMON))
      return;

   //--- Read request content
   string requestContent = "";
   while(!requestFileHandle.IsEnding())
   {
      requestContent += requestFileHandle.ReadString();
   }
   requestFileHandle.Close();

   if(StringLen(requestContent) == 0)
      return;

   if(EnableLogging)
      Print("Processing request: ", StringSubstr(requestContent, 0, 200));

   //--- Parse and handle request
   string response = HandleRequest(requestContent);

   //--- Write response
   WriteResponse(response);

   //--- Clear request file
   ClearRequestFile();
}

//+------------------------------------------------------------------+
//| Handle JSON request and route to appropriate handler              |
//+------------------------------------------------------------------+
string HandleRequest(string requestContent)
{
   CJAVal request;
   if(!request.Deserialize(requestContent))
      return CreateErrorResponse(400, "Invalid JSON format");

   //--- Extract action (endpoint)
   string action = request["action"].ToStr();

   if(EnableLogging)
      Print("Action: ", action);

   //--- Route to appropriate handler
   if(action == "order_send")
      return HandleOrderSend(request["data"]);
   else if(action == "order_check")
      return HandleOrderCheck(request["data"]);
   else if(action == "order_cancel")
      return HandleOrderCancel(request["data"]);
   else if(action == "positions_get")
      return HandlePositionsGet();
   else if(action == "orders_get")
      return HandleOrdersGet();
   else if(action == "account_info")
      return HandleAccountInfo();
   else if(action == "price_get")
      return HandlePriceGet(request["data"]["symbol"].ToStr());
   //--- Position management handlers
   else if(action == "position_close")
      return HandlePositionClose(request["data"]);
   else if(action == "position_close_partial")
      return HandlePositionClosePartial(request["data"]);
   else if(action == "position_modify")
      return HandlePositionModify(request["data"]);
   //--- Symbol information handlers
   else if(action == "symbol_info")
      return HandleSymbolInfo(request["data"]);
   else if(action == "symbol_select")
      return HandleSymbolSelect(request["data"]);
   else if(action == "symbols_get")
      return HandleSymbolsGet(request["data"]);
   //--- Historical data handlers
   else if(action == "candles_get")
      return HandleCandlesGet(request["data"]);
   else
      return CreateErrorResponse(404, "Not Found - Unknown action: " + action);
}

//+------------------------------------------------------------------+
//| Handle order send request                                          |
//+------------------------------------------------------------------+
string HandleOrderSend(CJAVal &data)
{
   //--- Extract order parameters
   string symbol = data["symbol"].ToStr();
   double volume = data["volume"].ToDbl();
   ENUM_ORDER_TYPE order_type = (ENUM_ORDER_TYPE)data["type"].ToInt();
   double price = data["price"].ToDbl();
   double sl = data["sl"].ToDbl();
   double tp = data["tp"].ToDbl();
   string comment = data["comment"].ToStr();
   int deviation = (int)data["deviation"].ToInt();
   ulong magic = (ulong)data["magic"].ToInt();

   //--- Set magic number if provided
   if(magic > 0)
      trade.SetExpertMagicNumber(magic);

   //--- Set deviation if provided
   if(deviation > 0)
      trade.SetDeviationInPoints(deviation);

   //--- Execute order
   bool result = false;
   if(order_type == ORDER_TYPE_BUY || order_type == ORDER_TYPE_SELL)
   {
      //--- Market order
      if(order_type == ORDER_TYPE_BUY)
         result = trade.Buy(volume, symbol, 0, sl, tp, comment);
      else
         result = trade.Sell(volume, symbol, 0, sl, tp, comment);
   }
   else
   {
      //--- Pending order
      result = trade.OrderOpen(symbol, order_type, volume, 0, price, sl, tp,
                                ORDER_TIME_GTC, 0, comment);
   }

   //--- Build response
   CJAVal response;
   response["success"] = result;
   response["retcode"] = (int)trade.ResultRetcode();
   response["retcode_description"] = trade.ResultRetcodeDescription();
   response["deal"] = (long)trade.ResultDeal();
   response["order"] = (long)trade.ResultOrder();
   response["volume"] = trade.ResultVolume();
   response["price"] = trade.ResultPrice();
   response["bid"] = trade.ResultBid();
   response["ask"] = trade.ResultAsk();
   response["comment"] = trade.ResultComment();

   return response.Serialize();
}

//+------------------------------------------------------------------+
//| Handle order check request                                         |
//+------------------------------------------------------------------+
string HandleOrderCheck(CJAVal &data)
{
   //--- Extract order parameters
   string symbol = data["symbol"].ToStr();
   double volume = data["volume"].ToDbl();
   ENUM_ORDER_TYPE order_type = (ENUM_ORDER_TYPE)data["type"].ToInt();
   double price = data["price"].ToDbl();

   //--- Create trade request
   MqlTradeRequest request = {};
   MqlTradeCheckResult check_result = {};

   request.action = TRADE_ACTION_DEAL;
   request.symbol = symbol;
   request.volume = volume;
   request.type = order_type;
   request.price = price;

   //--- Check order
   bool result = OrderCheck(request, check_result);

   //--- Build response
   CJAVal response;
   response["success"] = result;
   response["retcode"] = (int)check_result.retcode;
   response["balance"] = check_result.balance;
   response["equity"] = check_result.equity;
   response["profit"] = check_result.profit;
   response["margin"] = check_result.margin;
   response["margin_free"] = check_result.margin_free;
   response["margin_level"] = check_result.margin_level;
   response["comment"] = check_result.comment;

   return response.Serialize();
}

//+------------------------------------------------------------------+
//| Handle order cancel request                                        |
//+------------------------------------------------------------------+
string HandleOrderCancel(CJAVal &data)
{
   //--- Extract order ticket
   ulong ticket = data["ticket"].ToInt();

   //--- Cancel order
   bool result = trade.OrderDelete(ticket);

   //--- Build response
   CJAVal response;
   response["success"] = result;
   response["retcode"] = (int)trade.ResultRetcode();
   response["retcode_description"] = trade.ResultRetcodeDescription();

   return response.Serialize();
}

//+------------------------------------------------------------------+
//| Handle positions get request                                       |
//+------------------------------------------------------------------+
string HandlePositionsGet()
{
   CJAVal response;
   CJAVal positions;
   positions.m_type = jtARRAY;

   int total = PositionsTotal();
   for(int i = 0; i < total; i++)
   {
      if(positionInfo.SelectByIndex(i))
      {
         CJAVal position;
         position["ticket"] = (long)positionInfo.Ticket();
         position["symbol"] = positionInfo.Symbol();
         position["type"] = (int)positionInfo.PositionType();
         position["volume"] = positionInfo.Volume();
         position["price_open"] = positionInfo.PriceOpen();
         position["price_current"] = positionInfo.PriceCurrent();
         position["sl"] = positionInfo.StopLoss();
         position["tp"] = positionInfo.TakeProfit();
         position["profit"] = positionInfo.Profit();
         position["swap"] = positionInfo.Swap();
         position["commission"] = positionInfo.Commission();
         position["magic"] = (long)positionInfo.Magic();
         position["comment"] = positionInfo.Comment();

         positions.Add(position);
      }
   }

   response["success"] = true;
   response["count"] = total;
   response["positions"] = positions;

   return response.Serialize();
}

//+------------------------------------------------------------------+
//| Handle orders get request                                          |
//+------------------------------------------------------------------+
string HandleOrdersGet()
{
   CJAVal response;
   CJAVal orders;
   orders.m_type = jtARRAY;

   int total = OrdersTotal();
   for(int i = 0; i < total; i++)
   {
      if(orderInfo.SelectByIndex(i))
      {
         CJAVal order;
         order["ticket"] = (long)orderInfo.Ticket();
         order["symbol"] = orderInfo.Symbol();
         order["type"] = (int)orderInfo.OrderType();
         order["volume_initial"] = orderInfo.VolumeInitial();
         order["volume_current"] = orderInfo.VolumeCurrent();
         order["price_open"] = orderInfo.PriceOpen();
         order["sl"] = orderInfo.StopLoss();
         order["tp"] = orderInfo.TakeProfit();
         order["magic"] = (long)orderInfo.Magic();
         order["comment"] = orderInfo.Comment();
         order["state"] = (int)orderInfo.State();

         orders.Add(order);
      }
   }

   response["success"] = true;
   response["count"] = total;
   response["orders"] = orders;

   return response.Serialize();
}

//+------------------------------------------------------------------+
//| Handle account info request                                        |
//+------------------------------------------------------------------+
string HandleAccountInfo()
{
   CJAVal response;

   response["success"] = true;
   response["login"] = (long)AccountInfoInteger(ACCOUNT_LOGIN);
   response["balance"] = AccountInfoDouble(ACCOUNT_BALANCE);
   response["equity"] = AccountInfoDouble(ACCOUNT_EQUITY);
   response["profit"] = AccountInfoDouble(ACCOUNT_PROFIT);
   response["margin"] = AccountInfoDouble(ACCOUNT_MARGIN);
   response["margin_free"] = AccountInfoDouble(ACCOUNT_MARGIN_FREE);
   response["margin_level"] = AccountInfoDouble(ACCOUNT_MARGIN_LEVEL);
   response["currency"] = AccountInfoString(ACCOUNT_CURRENCY);
   response["name"] = AccountInfoString(ACCOUNT_NAME);
   response["server"] = AccountInfoString(ACCOUNT_SERVER);
   response["leverage"] = (long)AccountInfoInteger(ACCOUNT_LEVERAGE);

   return response.Serialize();
}

//+------------------------------------------------------------------+
//| Handle price get request                                           |
//+------------------------------------------------------------------+
string HandlePriceGet(string symbol)
{
   MqlTick tick;
   if(!SymbolInfoTick(symbol, tick))
      return CreateErrorResponse(404, "Symbol not found or no tick data: " + symbol);

   CJAVal response;
   response["success"] = true;
   response["symbol"] = symbol;
   response["bid"] = tick.bid;
   response["ask"] = tick.ask;
   response["last"] = tick.last;
   response["volume"] = (long)tick.volume;
   response["time"] = (long)tick.time;
   response["flags"] = (int)tick.flags;

   return response.Serialize();
}

//+------------------------------------------------------------------+
//| Create JSON response                                               |
//+------------------------------------------------------------------+
//| Handle position close request                                      |
//+------------------------------------------------------------------+
string HandlePositionClose(CJAVal &data)
{
   //--- Extract ticket
   ulong ticket = (ulong)data["ticket"].ToInt();
   
   if(EnableLogging)
      Print("Attempting to close position with ticket: ", ticket);
   
   //--- Close position
   if(!trade.PositionClose(ticket))
   {
      uint error_code = trade.ResultRetcode();
      string error_msg = trade.ResultComment();
      if(EnableLogging)
         Print("Failed to close position. Error: ", error_code, " - ", error_msg);
      return CreateErrorResponse((int)error_code, "Failed to close position: " + error_msg);
   }
   
   //--- Build successful response
   CJAVal result;
   result["success"] = true;
   result["retcode"] = (int)trade.ResultRetcode();
   result["order"] = (long)trade.ResultOrder();
   result["deal"] = (long)trade.ResultDeal();
   result["volume"] = trade.ResultVolume();
   result["price"] = trade.ResultPrice();
   result["comment"] = trade.ResultComment();
   
   if(EnableLogging)
      Print("Position closed successfully. Deal: ", trade.ResultDeal());
   
   return result.Serialize();
}

//+------------------------------------------------------------------+
//| Handle position close partial request                             |
//+------------------------------------------------------------------+
string HandlePositionClosePartial(CJAVal &data)
{
   //--- Extract parameters
   ulong ticket = (ulong)data["ticket"].ToInt();
   double volume = data["volume"].ToDbl();
   
   if(EnableLogging)
      Print("Attempting to partially close position ", ticket, " with volume ", volume);
   
   //--- Close partial position
   if(!trade.PositionClosePartial(ticket, volume))
   {
      uint error_code = trade.ResultRetcode();
      string error_msg = trade.ResultComment();
      if(EnableLogging)
         Print("Failed to partially close position. Error: ", error_code, " - ", error_msg);
      return CreateErrorResponse((int)error_code, "Failed to partially close position: " + error_msg);
   }
   
   //--- Build successful response
   CJAVal result;
   result["success"] = true;
   result["retcode"] = (int)trade.ResultRetcode();
   result["order"] = (long)trade.ResultOrder();
   result["deal"] = (long)trade.ResultDeal();
   result["volume_closed"] = trade.ResultVolume();
   result["price"] = trade.ResultPrice();
   result["comment"] = trade.ResultComment();
   
   if(EnableLogging)
      Print("Position partially closed. Volume: ", trade.ResultVolume(), ", Deal: ", trade.ResultDeal());
   
   return result.Serialize();
}

//+------------------------------------------------------------------+
//| Handle position modify request                                     |
//+------------------------------------------------------------------+
string HandlePositionModify(CJAVal &data)
{
   //--- Extract parameters
   ulong ticket = (ulong)data["ticket"].ToInt();
   double sl = data["sl"].ToDbl();
   double tp = data["tp"].ToDbl();
   
   if(EnableLogging)
      Print("Attempting to modify position ", ticket, " with SL=", sl, ", TP=", tp);
   
   //--- Modify position
   if(!trade.PositionModify(ticket, sl, tp))
   {
      uint error_code = trade.ResultRetcode();
      string error_msg = trade.ResultComment();
      if(EnableLogging)
         Print("Failed to modify position. Error: ", error_code, " - ", error_msg);
      return CreateErrorResponse((int)error_code, "Failed to modify position: " + error_msg);
   }
   
   //--- Build successful response
   CJAVal result;
   result["success"] = true;
   result["retcode"] = (int)trade.ResultRetcode();
   result["order"] = (long)trade.ResultOrder();
   result["sl"] = sl;
   result["tp"] = tp;
   result["comment"] = trade.ResultComment();
   
   if(EnableLogging)
      Print("Position modified successfully. SL: ", sl, ", TP: ", tp);
   
   return result.Serialize();
}

//+------------------------------------------------------------------+
//| Handle symbol info request                                         |
//+------------------------------------------------------------------+
string HandleSymbolInfo(CJAVal &data)
{
   //--- Extract symbol
   string symbol = data["symbol"].ToStr();
   
   if(EnableLogging)
      Print("Requesting symbol info for: ", symbol);
   
   //--- Get symbol info via tick
   MqlTick tick;
   if(!SymbolInfoTick(symbol, tick))
   {
      if(EnableLogging)
         Print("Failed to get symbol info for: ", symbol);
      return CreateErrorResponse(404, "Failed to get symbol info for: " + symbol);
   }
   
   //--- Build successful response
   CJAVal result;
   result["success"] = true;
   result["symbol"] = symbol;
   result["bid"] = tick.bid;
   result["ask"] = tick.ask;
   result["last"] = tick.last;
   result["volume"] = (long)tick.volume;
   result["time"] = (long)tick.time;
   
   //--- Additional symbol properties
   result["digits"] = (int)SymbolInfoInteger(symbol, SYMBOL_DIGITS);
   result["spread"] = (int)SymbolInfoInteger(symbol, SYMBOL_SPREAD);
   result["point"] = SymbolInfoDouble(symbol, SYMBOL_POINT);
   result["trade_contract_size"] = SymbolInfoDouble(symbol, SYMBOL_TRADE_CONTRACT_SIZE);
   
   if(EnableLogging)
      Print("Symbol info retrieved: ", symbol, " Bid=", tick.bid, " Ask=", tick.ask);
   
   return result.Serialize();
}

//+------------------------------------------------------------------+
//| Handle symbol select request                                       |
//+------------------------------------------------------------------+
string HandleSymbolSelect(CJAVal &data)
{
   //--- Extract parameters
   string symbol = data["symbol"].ToStr();
   bool enable = data["enable"].ToBool();
   
   if(EnableLogging)
      Print("Symbol select: ", symbol, " Enable=", enable);
   
   //--- Select/deselect symbol
   if(!SymbolSelect(symbol, enable))
   {
      if(EnableLogging)
         Print("Failed to select/deselect symbol: ", symbol);
      return CreateErrorResponse(400, "Failed to select/deselect symbol: " + symbol);
   }
   
   //--- Build successful response
   CJAVal result;
   result["success"] = true;
   result["symbol"] = symbol;
   result["selected"] = enable;
   
   if(EnableLogging)
      Print("Symbol ", (enable ? "enabled" : "disabled"), ": ", symbol);
   
   return result.Serialize();
}

//+------------------------------------------------------------------+
//| Handle symbols get request                                         |
//+------------------------------------------------------------------+
string HandleSymbolsGet(CJAVal &data)
{
   //--- Extract optional group filter
   string group = data["group"].ToStr();
   
   if(EnableLogging)
      Print("Getting symbols", (group != "" ? " for group: " + group : ""));
   
   //--- Build response
   CJAVal result;
   result["success"] = true;
   
   CJAVal symbols;
   int count = 0;
   
   //--- Iterate through all symbols
   for(int i = 0; i < SymbolsTotal(false); i++)
   {
      string symbolName = SymbolName(i, false);
      
      //--- Apply group filter if provided
      if(group != "" && StringFind(symbolName, group) < 0)
         continue;
      
      symbols[count] = symbolName;
      count++;
   }
   
   result["symbols"] = symbols;
   result["count"] = count;
   
   if(EnableLogging)
      Print("Found ", count, " symbols");
   
   return result.Serialize();
}

//+------------------------------------------------------------------+
//| Convert string to timeframe enum                                   |
//+------------------------------------------------------------------+
ENUM_TIMEFRAMES StringToTimeframe(string tf)
{
   if(tf == "M1") return PERIOD_M1;
   else if(tf == "M5") return PERIOD_M5;
   else if(tf == "M15") return PERIOD_M15;
   else if(tf == "M30") return PERIOD_M30;
   else if(tf == "H1") return PERIOD_H1;
   else if(tf == "H4") return PERIOD_H4;
   else if(tf == "D1") return PERIOD_D1;
   else if(tf == "W1") return PERIOD_W1;
   else if(tf == "MN1") return PERIOD_MN1;
   else return PERIOD_CURRENT;
}

//+------------------------------------------------------------------+
//| Handle candles get request (unified for all modes)                |
//+------------------------------------------------------------------+
string HandleCandlesGet(CJAVal &data)
{
   //--- Extract parameters
   string symbol = data["symbol"].ToStr();
   string tfString = data["timeframe"].ToStr();
   ENUM_TIMEFRAMES timeframe = StringToTimeframe(tfString);
   string mode = data["mode"].ToStr();
   
   if(EnableLogging)
      Print("Getting candles for ", symbol, " ", tfString, " mode: ", mode);
   
   //--- Get rates based on mode
   MqlRates rates[];
   int copied = 0;
   
   if(mode == "range")
   {
      datetime start = (datetime)data["start"].ToInt();
      datetime end = (datetime)data["end"].ToInt();
      if(EnableLogging)
         Print("Range mode: start=", start, " end=", end);
      copied = CopyRates(symbol, timeframe, start, end, rates);
   }
   else if(mode == "from")
   {
      datetime start = (datetime)data["start"].ToInt();
      int count = (int)data["count"].ToInt();
      if(EnableLogging)
         Print("From mode: start=", start, " count=", count);
      copied = CopyRates(symbol, timeframe, start, count, rates);
   }
   else if(mode == "from_pos")
   {
      int position = (int)data["position"].ToInt();
      int count = (int)data["count"].ToInt();
      if(EnableLogging)
         Print("From position mode: position=", position, " count=", count);
      copied = CopyRates(symbol, timeframe, position, count, rates);
   }
   else
   {
      return CreateErrorResponse(400, "Invalid mode: " + mode + ". Use 'range', 'from', or 'from_pos'");
   }
   
   //--- Check if data was copied
   if(copied <= 0)
   {
      if(EnableLogging)
         Print("Failed to copy rates. Error: ", GetLastError());
      return CreateErrorResponse(404, "Failed to copy rates for " + symbol);
   }
   
   //--- Build successful response
   CJAVal result;
   result["success"] = true;
   result["count"] = copied;
   result["symbol"] = symbol;
   result["timeframe"] = tfString;
   
   //--- Serialize candles array DIRECTLY on result["candles"]
   // NOTE: JAson.mqh bug - cannot assign array to key (key becomes empty string)
   // Must build array in-place using result["candles"].Add()
   result["candles"].m_type = jtARRAY;
   for(int i = 0; i < copied; i++)
   {
      CJAVal candle;
      candle["time"] = (long)rates[i].time;
      candle["open"] = rates[i].open;
      candle["high"] = rates[i].high;
      candle["low"] = rates[i].low;
      candle["close"] = rates[i].close;
      candle["volume"] = (long)rates[i].tick_volume;
      result["candles"].Add(candle);  // Build directly on result["candles"]
   }
   
   if(EnableLogging)
      Print("Copied ", copied, " candles for ", symbol);
   
   return result.Serialize();
}

//+------------------------------------------------------------------+
string CreateJSONResponse(string json_body)
{
   return json_body;
}

//+------------------------------------------------------------------+
//| Create error response                                              |
//+------------------------------------------------------------------+
string CreateErrorResponse(int code, string message)
{
   CJAVal json;
   json["success"] = false;
   json["error_code"] = code;
   json["error_message"] = message;

   return json.Serialize();
}

//+------------------------------------------------------------------+
//| Write response to file                                             |
//+------------------------------------------------------------------+
void WriteResponse(string response)
{
   if(responseFileHandle.Open(ResponseFile, FILE_WRITE|FILE_TXT|FILE_ANSI|FILE_COMMON))
   {
      responseFileHandle.WriteString(response);
      responseFileHandle.Close();

      if(EnableLogging)
         Print("Response written: ", StringSubstr(response, 0, 200));
   }
   else
   {
      Print("Failed to write response file: ", GetLastError());
   }
}

//+------------------------------------------------------------------+
//| Clear request file                                                 |
//+------------------------------------------------------------------+
void ClearRequestFile()
{
   if(requestFileHandle.Open(RequestFile, FILE_WRITE|FILE_TXT|FILE_ANSI|FILE_COMMON))
   {
      requestFileHandle.Close();
   }
}
//+------------------------------------------------------------------+
