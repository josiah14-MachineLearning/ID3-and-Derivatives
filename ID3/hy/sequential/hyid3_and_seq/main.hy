(import [numpy :as np])
(import [pandas :as pd])
(require [helpers [*]])

(setv a (np.array [[0 1 2 3 4 5] [10 11 12 13 14 15] [20 21 22 23 24 25] [30 31 32 33 34 35] [40 41 42 43 44 45] [50 51 52 53 54 55]]))
(print a)
(print #s(a [0 1 2 3 4] [1 2 3 4 5]))

(defn entropy [total_records
               value_frequencies
               &optional [log_base 2]]
    (setv item_probs
      (/ value_frequencies total_records))
    (- (.sum
         (/
           (* item_probs
              (np.log item_probs))
           (np.log log_base)))))

(defn frame_entropy [df
                     target_feature]
    (setv grouped_df (.groupby df target_feature))
    (setv counts
      (map (fn [k]
             (len
               (. (.get_group grouped_df k) index)))
           (grouped_df.indices.keys)))

    (entropy (len df.index)
             (np.array (list counts))))

(defn remaining_entropy [original_df
                         target_feature
                         grouped_df]
    (defn weighted_group_entropy [df]
      (* (/ (len df.index)
            (len original_df.index))
         (frame_entropy df target_feature)))

    (setv grouped_frames
      (map grouped_df.get_group
           (grouped_df.indices.keys)))

    (.sum
      (np.array
        (list
          (map weighted_group_entropy
               grouped_frames)))))

(defn information_gain [target_feature
                        original_entropy
                        original_df
                        grouped_df]
  (- original_entropy
     (remaining_entropy
       original_df target_feature grouped_df)))

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
(print)

(print)
(setv eco_veg_df
    (.drop (pd.DataFrame ecological_vegetation_data) "Id" :axis 1))
(print (frame_entropy eco_veg_df "Vegetation"))
(print
    (remaining_entropy
        eco_veg_df "Vegetation" (.groupby eco_veg_df "Elevation")))
(print
  (information_gain
    "Vegetation"
    (frame_entropy eco_veg_df "Vegetation")
    eco_veg_df
    (.groupby eco_veg_df "Elevation")))
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
(print)

(print)
(print (frame_entropy acute_inflammations_df "RenalPelvisNephritis"))
(print
    (remaining_entropy
        acute_inflammations_df
        "RenalPelvisNephritis"
        (.groupby acute_inflammations_df "Nausea")))
(print
  (information_gain
    "RenalPelvisNephritis"
    (frame_entropy acute_inflammations_df "RenalPelvisNephritis")
    acute_inflammations_df
    (.groupby acute_inflammations_df "Nausea")))

