(import [numpy :as np])
(import [pandas :as pd])
(require [helpers [*]])

(setv a (np.array [[0 1 2 3 4 5] [10 11 12 13 14 15] [20 21 22 23 24 25] [30 31 32 33 34 35] [40 41 42 43 44 45] [50 51 52 53 54 55]]))
(print a)
(print #s(a [0 1 2 3 4] [1 2 3 4 5]))

(setv spam_analysis_data {
    "SpamId" [376 489 541 693 782 976]
    "SuspciousWords" [True True True False False False]
    "UnknownSender" [False True True True False False]
    "Images" [True False False True False False]
    "SpamClass" ["spam" "spam" "spam" "ham" "ham" "ham"]})

(setv ecological_vegetation_data {
    "Id" [1 2 3 4 5 6 7]
    "Stream" [False True True False False True True]
    "Slope" ["steep" "moderate" "steep" "steep" "flat" "steep" "steep"]
    "Elevation" ["high" "low" "medium" "medium" "high" "highest" "high"]
    "Vegetation" ["chaparal" "riparian" "riparian" "chaparal" "conifer" "conifer" "chaparal"]})

(print ecological_vegetation_data)

(defn entropy [total_records value_frequencies &optional [log_base 2]]
    (setv item_probs (/ value_frequencies total_records))
    (- (.sum (/ (* item_probs (np.log item_probs))
                (np.log log_base)))))

(defn frame_entropy [df target_feature]
    (setv grouped_df (.groupby df target_feature))
    (setv counts
        (map (fn [k]
                 (len (. (.get_group grouped_df k) index)))
             ((. grouped_df indices keys))))
    (entropy (len df.index) (np.array (list counts))))

(setv spam_df (.drop (pd.DataFrame spam_analysis_data) "SpamId" :axis 1))
(print (frame_entropy spam_df "SpamClass"))

(setv eco_veg_df
    (.drop (pd.DataFrame ecological_vegetation_data) "Id" :axis 1))
(print (frame_entropy eco_veg_df "Vegetation"))
