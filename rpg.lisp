;;; Weapons

(defclass weapon ()
	((damage
	  :initarg :damage
	  :reader damage
	  :initform (error "Must have damage."))))

(defmacro defproperty (name)
	`(defclass ,name (weapon)
		 ()))

(defproperty piercing)

(defproperty reach)

(defproperty ranged)

(defproperty void)

; (defparameter +weapon-props+ '(:piercing :reach :ranged :void))

(defmacro defweapon (wep-name default-dmg &optional (properties '(weapon)))
	`(defclass ,wep-name (,@properties)
		 ((damage
			 :initarg :damage
			 :reader damage
			 :initform ,default-dmg))))

(defweapon natural-weapon 2)

(defweapon sword 5)

(defweapon greatsword 5 (piercing))

(defweapon spear 4 (reach))

(defweapon shortbow 3 (ranged))

(defweapon obsidian-dagger 2 (void))

;;; Combatants

(defclass combatant ()
    ((hp
	  :initarg :hp
	  :accessor hp)
	 (armor
	  :initarg :armor
	  :accessor armor)
	 (ward
	  :initarg :ward
	  :accessor ward)
	 (max-hp
	  :initarg :max-hp
	  :initform (error "Must have max-hp.")
	  :reader max-hp)
	 (max-armor
	  :initarg :max-armor
	  :initform 0
	  :reader max-armor)
	 (max-ward
	  :initarg :max-ward
	  :initform 0
	  :reader max-ward)
	 (spells
	  :initarg :spells
	  :initform (list)
	  :accessor spells)
	 (weapons
	  :initarg :weapons
	  :initform (list)
	  :accessor weapons)))

(defmacro bind-default (obj &rest specifiers)
	`(progn
		,@(mapcar (lambda (lst)
						(let ((slot (first lst))
							   (def-form (second lst)))
							`(when (not (slot-boundp ,obj ,slot))
								(setf (slot-value ,obj ,slot) ,def-form))))
					specifiers)))

(defmethod initialize-instance :after ((combatant combatant) &key)
	(bind-default combatant ('hp (max-hp combatant))
							('armor (max-armor combatant))
							('ward (max-ward combatant))))

(defun health (combatant)
	(+ (hp combatant) (ward combatant) (armor combatant)))

;;; Attacking

(defgeneric attack (attacker defender))

(defmethod attack ((attacker combatant) defender)
	(attack (first (weapons attacker)) defender))

(defun hits-armor-p (weapon defender)
	(> (armor defender) 0))

(defun rolls-armor-p (weapon defender)
	"Return T if the damage will exceed the armor."
	(<= (/ (damage weapon) 2) (armor defender)))

(defun remaining-armor (weapon defender)
  (- (armor defender) (/ (damage weapon))))

(defun dmg-after-armor (weapon defender)
	"Calculate the damage left after armor")

(defmethod attack ((weapon weapon) defender)
	(let ((dmg (damage weapon)))
		(cond
			;; If there is armor
			((and (hits-armor-p weapon defender) (rolls-armor-p weapon defender))
				(setf (armor defender) (remaining-armor weapon defender)))
			((hits-armor-p weapon defender)
				(let ((new-dmg (- dmg (* 2 (armor defender)))))
					(setf (armor defender) 0)
					(attack defender new-dmg)))
			;; If there is ward
			((> (ward defender) 0) (setf (ward defender) (max 0 (- (ward defender) dmg))))
			;; If there is just HP
			(t (setf (hp defender) (- (hp defender) dmg)))))
	defender)

(defmethod attack ((dmg number) combatant)
	(attack (natural-weapon dmg) combatant))

;;; Debug/CLI

(defun print-health (combatant)
	(format t "HP: ~a~%Ward: ~a~%Armor: ~a~%" (hp combatant) (ward combatant) (armor combatant))
	combatant)

(defun print-weapon (weapon)
	(format t "Damage: ~a~%" (damage weapon))
	weapon)

(defun print-weapons (combatant)
	(mapcar #'print-weapon (weapons combatant))
	combatant)

;;; Tests

(defparameter +slime+ (make-instance 'combatant
									 :max-hp 5
									 :max-armor 0
									 :max-ward 0
									 :weapons (list (make-instance 'weapon :damage 2))))

(defparameter +wizard+ (make-instance 'combatant
									  :max-hp 3
									  :max-ward 1
									  :max-armor 0
									  :weapons (list (make-instance 'shortbow :damage 3))))

(defparameter +minotaur+ (make-instance 'combatant
										:max-hp 10
										:max-ward 0
										:max-armor 4
										:weapons (list (make-instance 'greatsword :damage 6))))
