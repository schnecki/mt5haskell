//+------------------------------------------------------------------+
//| Script to print MT5 enum values                                  |
//+------------------------------------------------------------------+
#property strict

void OnStart()
{
    Print("=== MT5 Enum Values ===");
    
    Print("\nOrderType:");
    Print("  ORDER_TYPE_BUY           = ", ORDER_TYPE_BUY);
    Print("  ORDER_TYPE_SELL          = ", ORDER_TYPE_SELL);
    Print("  ORDER_TYPE_BUY_LIMIT     = ", ORDER_TYPE_BUY_LIMIT);
    Print("  ORDER_TYPE_SELL_LIMIT    = ", ORDER_TYPE_SELL_LIMIT);
    Print("  ORDER_TYPE_BUY_STOP      = ", ORDER_TYPE_BUY_STOP);
    Print("  ORDER_TYPE_SELL_STOP     = ", ORDER_TYPE_SELL_STOP);
    Print("  ORDER_TYPE_BUY_STOP_LIMIT  = ", ORDER_TYPE_BUY_STOP_LIMIT);
    Print("  ORDER_TYPE_SELL_STOP_LIMIT = ", ORDER_TYPE_SELL_STOP_LIMIT);
    Print("  ORDER_TYPE_CLOSE_BY      = ", ORDER_TYPE_CLOSE_BY);
    
    Print("\nOrderTypeFilling:");
    Print("  ORDER_FILLING_FOK    = ", ORDER_FILLING_FOK);
    Print("  ORDER_FILLING_IOC    = ", ORDER_FILLING_IOC);
    Print("  ORDER_FILLING_RETURN = ", ORDER_FILLING_RETURN);
    
    Print("\nOrderTypeTime:");
    Print("  ORDER_TIME_GTC           = ", ORDER_TIME_GTC);
    Print("  ORDER_TIME_DAY           = ", ORDER_TIME_DAY);
    Print("  ORDER_TIME_SPECIFIED     = ", ORDER_TIME_SPECIFIED);
    Print("  ORDER_TIME_SPECIFIED_DAY = ", ORDER_TIME_SPECIFIED_DAY);
    
    Print("\nTradeAction:");
    Print("  TRADE_ACTION_DEAL     = ", TRADE_ACTION_DEAL);
    Print("  TRADE_ACTION_PENDING  = ", TRADE_ACTION_PENDING);
    Print("  TRADE_ACTION_SLTP     = ", TRADE_ACTION_SLTP);
    Print("  TRADE_ACTION_MODIFY   = ", TRADE_ACTION_MODIFY);
    Print("  TRADE_ACTION_REMOVE   = ", TRADE_ACTION_REMOVE);
    Print("  TRADE_ACTION_CLOSE_BY = ", TRADE_ACTION_CLOSE_BY);
}
