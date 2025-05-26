// use crate::parser::{Expression, Visitor};

// pub struct Printer;

// impl Visitor<String> for Printer {
//     fn visit_expression(&mut self, expr: &Expression) -> String {
//         match expr {
//             Expression::Assign(token, value) => {
//                 format!("{} = {}", token.lexeme, self.visit_expression(value))
//             }
//             Expression::Binary(left, op, right) => {
//                 format!(
//                     "({} {} {})",
//                     self.visit_expression(left),
//                     op.lexeme,
//                     self.visit_expression(right)
//                 )
//             }
//             Expression::Logical(left, op, right) => {
//                 format!(
//                     "({} {} {})",
//                     self.visit_expression(left),
//                     op.lexeme,
//                     self.visit_expression(right)
//                 )
//             }
//             Expression::Unary(op, right) => {
//                 format!("({} {})", op.lexeme, self.visit_expression(right))
//             }
//             Expression::Literal(token) => token.lexeme.clone(),
//             Expression::Grouping(expr) => format!("({})", self.visit_expression(expr)),
//         }
//     }
// }
