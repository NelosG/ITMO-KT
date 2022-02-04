package types;

import java.util.List;

public record Rule(String name,
                   List<Arg> arguments,
                   String returnType,
                   List<Production> productions) {


//    @Override
//    public String toString() {
//        StringBuilder sb = new StringBuilder();
//        sb.append(name);
//        if(arguments.size() != 0) {
//            sb.append("(");
//            for (var arg : arguments) {
//                sb.append(arg.type()).append(" ").append(arg.name());
//                sb.append(',');
//            }
//            sb.append(")");
//        }
//        if(returnType != null) {
//            sb.append(": ").append(returnType);
//        }
//        sb.append(" \n\t-> ");
//
//        addProd(sb, productions);
//        return sb.toString() + ";";
//    }
//
//    private void addProd(StringBuilder sb, List<Production> production) {
//        for (var prod: production) {
//
//            for(ExtendedElement el : prod.extendedProduction()) {
//                if(el instanceof Code) {
//                    sb.append("{ ").append(((Code) el).code()).append(" }");
//                } else {
//                    addElement(sb, ((ProductionElement)el).element());
//                }
//                sb.append(" ");
//            }
//            if(prod.returnExpression() != null) {
//                sb.append("[ return ").append(prod.returnExpression()).append("; ]");
//            }
//            sb.append("\n\t| ");
//        }
//    }
//
//    private void addElement(StringBuilder sb, Element el){
//
//        sb.append(el.name);
//        if(el instanceof NonTerm) {
//            NonTerm term = (NonTerm) el;
//            if(term.callAttributes.size() != 0) {
//                sb.append("(");
//                for (var arg: term.callAttributes) {
//                    sb.append(arg).append(", ");
//                }
//                sb.append(")");
//            }
//        }
//    }
}
