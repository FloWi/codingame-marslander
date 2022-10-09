import numpy as np

from MarsLanderGymEnv import MarsLanderGymEnv
import time


with MarsLanderGymEnv() as env:
    episodes = 50

    run_times = []

    start = time.time()

    for episode in range(episodes):
        s = time.time()
        done = False
        obs = env.reset()
        while True:  # not done:
            random_action = env.action_space.sample()
            print("action", random_action)
            obs, reward, done, truncated, info = env.step(random_action)
            print('reward', reward)
            if done:
                print("is_done")
                break
        e = time.time()
        run_times.append([episode, (e - s) * 1000])

    end = time.time()

    run_times = np.array(run_times)
    print(run_times)
