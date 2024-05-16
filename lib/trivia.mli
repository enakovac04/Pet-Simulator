(** type [trivia] stores a question and its answer *)
type trivia = {
  question : string;
  answer : string;
}

(** [get_question] returns the question of a given trivia question *)
val get_question : trivia -> string

(** [get_answer] given a trivia question returns the answer *)
val get_answer : trivia -> string

(** [correct] returns whether or not the given answer [ans] is correct 
 If correct the given animal [animal] gains $1
 If incorrect [animal] loses $1 or if at $0 stays at $0 *)
val correct : trivia -> string -> Pet.animal -> bool

(** [trivia_questions] stores all of the questions and each answer for the game *)
val trivia_questions : trivia list