(import [numpy :as np])
(import [pandas :as pd])
(require [helpers [*]])


(defn entropy [total_records
               value_frequencies
               &optional [log_base 2]]
  (setv item_probs
    (/ value_frequencies total_records))
  (setv weighted_logs_of_probabilities
    (/ (* item_probs
          ((. np log) item_probs))
       ((. np log) log_base)))
  (-> weighted_logs_of_probabilities .sum -))


(defn frame_entropy [df
                     target_feature]
  (setv grouped_df (.groupby df target_feature))
  (setv counts
    (map (fn [k]
           (len
             (. (.get_group grouped_df k) index)))
         ((. grouped_df indices keys))))

  (->> counts list ((. np array))
       (entropy (len (. df index)))))


(defn remaining_entropy [original_df
                         target_feature
                         grouped_df]
  (defn weighted_group_entropy [df]
    (* (/ (len (. df index))
          (len (. original_df index)))
       (frame_entropy df target_feature)))

  (setv grouped_frames
    (map grouped_df.get_group
         ((. grouped_df indices keys))))

  (->> grouped_frames (map weighted_group_entropy) list ((. np array)) .sum))


(defn information_gain [target_feature
                        original_entropy
                        original_df
                        grouped_df]
  (- original_entropy
     (remaining_entropy
       original_df target_feature grouped_df)))


(defn find_most_informative_feature [target_feature df]
  (setv original_entropy (frame_entropy df target_feature))
  (defn calc_IG [descriptive_feature]
    (setv grouped_df ((. df groupby) descriptive_feature))
    (, descriptive_feature
       (information_gain target_feature original_entropy df grouped_df)
       grouped_df))

  (defn keep_greatest_IG [acc_df
                          next_descriptive_feature]
    (setv next_df (calc_IG next_descriptive_feature))
    (if (>= (get acc_df 1) (get next_df 1))
      acc_df
      next_df))

  (setv descriptive_features
    (-> (. (.drop df target_feature :axis 1) columns) list))
  (setv (get descriptive_features 0)
    (calc_IG (get descriptive_features 0)))

  ;; If there's only 1 descriptive feature, reduce will just return the only item
  ;; in the list, so we don't need an if-statement here to provide the shortcut
  ;; explicitly mentioned in the pseudocode algorithms in textbooks and online
  ;; which describe the ID3 algorithm.
  (reduce keep_greatest_IG descriptive_features))


(defn id3 [target_feature df]
  "
    High-level algorithm summary:

    1. If all of the target_feature values in the training set DataFrame are the
       same value, return that value as the new leaf.
    2. If there is only one descriptive_feature left to split on, split by it and
       make a new node which is a tuple where the first tuple value is the name
       of the descriptive_feature column, and the second is a dictionary where
       the keys are each unique value of the descriptive_feature column, and the
       values are the mode of the target_feature of that grouping of the
       descriptive_feature value.  Also add an `otherwise` key whose value is
       the mode of the unsplit frame's target_feature column in case real-world
       data contains unique values of the descriptive feature that got excluded
       via former iterations of splitting the training set to build this model.
    3. Otherwise, identify the descriptive_feature which yields the greatest
       Information Gain and use it to split the training set DataFrame.  Make
       a new node which is a tuple where the first tuple value is the name of
       the descriptive_feature column, and the second is the a dictionary where
       the keys are each unique value of the descriptive_feature column, and
       the values are the result of running this id3 function recursively over
       the DataFrame for the group at the key (which is the descriptive_feature
       value).  As in step 2, also add an `otherwise` key whose value is
       the mode of the unsplit frame's target_feature column in case real-world
       data contains unique values of the descriptive feature that got excluded
       via former iterations of splitting the training set to build this model.

    :param target_feature: The name of the column this model should try to
        predict.
    :param df: The training set to use to build this decision tree model.

    :return: A tuple which is a decision tree structure.  To explain the
        mypy type signature, the Dict's first Any represents the type of
        the descriptive_feature at that node, and the second Any could be either
        another tree node, which would have signature Tuple[str, Dict[Any, Any]],
        or it could be a leaf, which would just be a str value which is the value
        predicted at the end of that traversal of the tree.

        Here are some sample trees:
        ; simple Ham or Spam prediction example
        ('SuspiciousWords', {True: 'spam', False: 'ham'})

        ; sample Ecological Vegetation decision tree
        ('Elevation', {
            'low': 'riparian',
            'highest': 'conifer',
            'medium': ('Stream', {
                True: 'riparian',
                False: 'chaparal'
            }),
            'high': ('Slope', {
                'flat': 'conifer',
                'steep': 'chaparal',
                # below, 'moderate' was eliminated through splitting, so the
                # mode of the target_level of the frame unsplit by 'Slope' is
                # assumed via 'otherwise' for any value other than 'flat' or
                # 'steep', which covers 'moderate' since it got excluded by
                # the split.
                'otherwise': 'chaparal'
            })
        })
  "
  (comment
    (defn mk_fxn [fxn_name]
      `(defn ~fxn_name [arg1 arg2]
         (+ arg1 arg2)))
    (eval (mk_fxn `add_two))
    (add_two 1 2)
    ; => 3
    )
  (pass))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test the ID3 functions on some datasets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setv spam_analysis_data {
  "SpamId" [376 489 541 693 782 976]
  "SuspiciousWords" [True True True False False False]
  "UnknownSender" [False True True True False False]
  "Images" [True False False True False False]
  "SpamClass" ["spam" "spam" "spam" "ham" "ham" "ham"]})

(setv ecological_vegetation_data {
  "Id" [1 2 3 4 5 6 7]
  "Stream" [False True True False False True True]
  "Slope" ["steep" "moderate" "steep" "steep" "flat" "steep" "steep"]
  "Elevation" ["high" "low" "medium" "medium" "high" "highest" "high"]
  "Vegetation" ["chaparal" "riparian" "riparian" "chaparal" "conifer" "conifer" "chaparal"]})

(print)
(setv spam_df (.drop (pd.DataFrame spam_analysis_data) "SpamId" :axis 1))
(print (frame_entropy spam_df "SpamClass"))
(print (remaining_entropy spam_df "SpamClass" (.groupby spam_df "SuspiciousWords")))
(print
  (information_gain
    "SpamClass"
    (frame_entropy spam_df "SpamClass")
    spam_df
    (.groupby spam_df "SuspiciousWords")))
(print (find_most_informative_feature "SpamClass" spam_df))
(print)

(print)
(setv eco_veg_df
  (.drop (pd.DataFrame ecological_vegetation_data) "Id" :axis 1))
(print (frame_entropy eco_veg_df "Vegetation"))
(print
  (remaining_entropy eco_veg_df "Vegetation" (.groupby eco_veg_df "Elevation")))
(print
  (information_gain
    "Vegetation"
    (frame_entropy eco_veg_df "Vegetation")
    eco_veg_df
    (.groupby eco_veg_df "Elevation")))
(print
  (find_most_informative_feature "Vegetation" eco_veg_df))
(print)

(print)
(setv acute_inflammations_df
  (do
    (setv raw_df
      (pd.read_csv "../../../datasets/acute_diagnoses/diagnosis.data"
        :sep "\t" :lineterminator "\n" :header None :encoding "utf-8"))
    (.drop
      (.rename raw_df :columns {
        0 "Temperature"
        1 "Nausea"
        2 "LumbarPain"
        3 "UrinePushing"
        4 "MicturationPains"
        5 "UrethreaBurning"
        6 "BladderInflammation"
        7 "RenalPelvisNephritis"})
      "Temperature" :axis 1)))
(print (frame_entropy acute_inflammations_df "BladderInflammation"))
(print
  (remaining_entropy
    acute_inflammations_df
    "BladderInflammation"
    (.groupby acute_inflammations_df "UrinePushing")))
(print
  (information_gain
    "BladderInflammation"
    (frame_entropy acute_inflammations_df "BladderInflammation")
    acute_inflammations_df
    (.groupby acute_inflammations_df "UrinePushing")))
(setv acute_inf_predict_bladder_inf_df
  (.drop acute_inflammations_df "RenalPelvisNephritis" :axis 1))
(print
  (find_most_informative_feature
    "BladderInflammation" acute_inf_predict_bladder_inf_df))
(print)

(print)
(print (frame_entropy acute_inflammations_df "RenalPelvisNephritis"))
(print
  (remaining_entropy
    acute_inflammations_df
    "RenalPelvisNephritis"
    (.groupby acute_inflammations_df "LumbarPain")))
(print
  (information_gain
    "RenalPelvisNephritis"
    (frame_entropy acute_inflammations_df "RenalPelvisNephritis")
    acute_inflammations_df
    (.groupby acute_inflammations_df "LumbarPain")))
(setv acute_inf_predict_renal_nephritis
  (.drop acute_inflammations_df "BladderInflammation" :axis 1))
(print
  (find_most_informative_feature
    "RenalPelvisNephritis" acute_inf_predict_renal_nephritis))
