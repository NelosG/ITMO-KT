package types;

import java.util.List;

public record Production(List<ExtendedElement> extendedProduction,
                         String returnExpression) {
}
