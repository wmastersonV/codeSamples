import pandas as pd
import math as m
import numpy as np

# assumptions: input is either 1 or 0
# last column of input is the "class"

#1 = split right
#0 = split left

def calc_entropy(last_column):
    #input: data
    #output: entropy
    # Entropy = - p(a)*log2(p(a)) - p(b)*log2(p(b))

    #grab class column
    a = float(sum(last_column == 1))/float(len(last_column))
    b = 1 - a
    Entropy = - a * m.log(a, 2.0) - b * m.log(b, 2)
    return(Entropy)


train = pd.read_csv('/Users/dmaste/Desktop/train.txt', sep="\t")
test = pd.read_csv('/Users/dmaste/Desktop/test.txt', sep="\t")


last_column = train.ix[:, -1]
dat = train
ent_vec = np.empty(dat.shape[1] - 1 )


def find_split(dat):

    # calculate information gain for each attribute, determine attribute to split on
    for i in range(1, len(train.columns) - 1):
        print dat.columns[i]
        print i
        #split dataset into two parts
        one_index = dat.ix[:, i] == 1
        zero_index = dat.ix[:, i] == 0

        # split dataset into two parts
        class_when_feature_i_equal_1 = last_column[one_index]
        class_when_feature_i_equal_0 = last_column[zero_index]

        #calc weights
        w_1 = float(len(class_when_feature_i_equal_1)) / (float(len(class_when_feature_i_equal_1)) +
                                                          float(len(class_when_feature_i_equal_0)))
        w_0 = float(len(class_when_feature_i_equal_0)) / (float(len(class_when_feature_i_equal_1)) +
                                                          float(len(class_when_feature_i_equal_0)))

        #calculate entropy for left and right tree node
        Entropy_After = w_1 * calc_entropy(class_when_feature_i_equal_1) + w_0 * calc_entropy(class_when_feature_i_equal_0)
        print(Entropy_After)

        #store entropies and find max later
        ent_vec[i] = Entropy_After

    min_entropy = min(ent_vec)
    feature_to_split =






