data PropLogic = Not         PropLogic
               | And         PropLogic PropLogic
               | Or          PropLogic PropLogic
               | Implies     PropLogic PropLogic
               | IfAndOnlyIf PropLogic PropLogic
               | Top | Bottom
               | P | Q | S | R
