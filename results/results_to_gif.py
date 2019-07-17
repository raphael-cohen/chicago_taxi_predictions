import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as anim
# from PIL import Image
from PIL import Image



def main():

    gif_name = 'bw_price_pred.gif'
    dir = "../5_profit_maximiser/Results/BWPreds/"
    # dir = "airport_pickup"

    img_list = load_images_from_folder(dir)
    print(img_list[0])

    m = 1000
    n = 1000

    animated_gif = AnimatedGif(size=(m, n))

    for i, n in enumerate(img_list):
        # animated_gif.add(n, label="{0}H".format(int(i/4)))
        animated_gif.add(n, label="{0}th time period".format(i))

    animated_gif.save(gif_name)



def load_images_from_folder(folder):
    images = []
    img_n = []

    for filename in os.listdir(folder):
        img_n.append(filename)

    #We want to sort the filenames to have an ordered gif
    img_n.sort(key=lambda x: int(x[0:-4]))
    print(img_n)

    for filename in img_n:
        img = Image.open(os.path.join(folder,filename))
        images.append(img)
    return images



class AnimatedGif:
    def __init__(self, size=(640, 480)):
        self.fig = plt.figure()
        self.fig.set_size_inches(size[0] / 100, size[1] / 100)
        ax = self.fig.add_axes([0, 0, 1, 1], frameon=False, aspect=1)
        ax.set_xticks([])
        ax.set_yticks([])
        self.images = []

    def add(self, image, label=''):
        plt_im = plt.imshow(image, animated=True)#, vmin=0, vmax=1)
        plt_txt = plt.text(1500, 200, label, color='black', fontsize=35)
        self.images.append([plt_im, plt_txt])

    def save(self, filename):
        animation = anim.ArtistAnimation(self.fig, self.images)
        animation.save(filename, writer='imagemagick', fps=12)


if __name__ == '__main__':
    main()
