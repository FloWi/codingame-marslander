import json
import subprocess
from typing import Optional

import gym
import numpy as np
from gym import spaces
from gym.spaces import Dict, Discrete


class MarsLanderGymEnv(gym.Env):
    """Mars Lander environment that follows gym interface"""

    def __init__(self):
        super(MarsLanderGymEnv, self).__init__()

        self.landing_coords = []
        self.level_name = None
        self.num_landing_coords = None
        self.proc = None
        self.done = False

        max_rotate = 90
        rotation_action_space = Discrete(2 * max_rotate + 1, start=-max_rotate)  # to include 0
        power_action_space = Discrete(5)
        action_space = Dict({"rotation": rotation_action_space, "power": power_action_space})

        self.action_space = action_space

        low = np.array(
                [
                    0,  # x
                    0,  # y
                    0,  # fuel
                    0,  # power
                    -90,  # rotation
                ]
        ).astype(np.float32)
        high = np.array(
                [
                    7000,  # x
                    3000,  # y
                    1000,  # fuel
                    4,  # power
                    90,  # rotation
                ]
        ).astype(np.float32)
        self.observation_space = spaces.Box(low, high)

    def step(self, action):
        poll = self.proc.poll()
        if poll is None:
            # print("sending command")
            command = f"{action['rotation']} {action['power']}"
            self.proc.stdin.writelines([f"{command}\n"])
            print(f'command "{command}" sent')

            state_str = self.proc.stdout.readline()
            print(f'state_str trimmed: {state_str[0: 80]}')
            new_state = json.loads(state_str)
            reward = 1000 - new_state['fuel']

            if self.done:
                reward = - 10

            info = {}

            if self.is_done(new_state):
                self.done = True

            truncated = False
            return self.create_observation(new_state), reward, self.done, truncated, info

    def reset(
            self,
            *,
            seed: Optional[int] = None,
            options: Optional[dict] = None,
    ):
        super().reset(seed=seed)

        self.done = False

        if not seed:
            seed = np.random.randint(0, 42)

        if not self.proc:
            node_path = "/usr/local/bin/node"
            script_path = "../cli/target/scala-2.13/cli-opt/main.js"
            self.proc = subprocess.Popen([node_path, script_path], bufsize=1, stderr=subprocess.PIPE, stdout=subprocess.PIPE, stdin=subprocess.PIPE, text=True)

        # initiate with seed
        line = self.proc.stdout.readline()
        if ("seed" not in line):
            raise ValueError(f"expected prompt to enter seed. Was '{line}'")
        else:
            self.proc.stdin.writelines([f"{seed}\n"])

        # initiate game
        self.level_name = self.proc.stdout.readline()
        # print(f"Level: {level_name}")
        self.num_landing_coords = int(self.proc.stdout.readline())
        self.landing_coords = []

        for _ in range(self.num_landing_coords):
            self.landing_coords.append(self.proc.stdout.readline())

        initial_state = json.loads(self.proc.stdout.readline())

        # create observation:
        observation = self.create_observation(initial_state)

        return observation, {}  # reward, done, info can't be included

    def render(self, mode='human'):
        ...

    def close(self):
        if self.proc:
            self.proc.kill()

    def create_observation(self, state):
        return np.array([
            state["x"],
            state["y"],
            state["fuel"],
            state["power"],
            state["rotation"],
            # state.hSpeed,
            # state.vSpeed,
            # state.horizontalDistanceLandingArea,
            # state.verticalDistanceLandingArea,
            # state.distanceLandingArea,
        ])

    @staticmethod
    def is_done(state):
        return state["isCrashed"] or state["isLanded"] or state["isOffLimits"] or state["isOutOfFuel"]

# node_path = "/usr/local/bin/node"
# script_path = "../cli/target/scala-2.13/cli-opt/main.js"
# proc = subprocess.Popen([node_path, script_path], bufsize=1, stderr=subprocess.PIPE, stdout=subprocess.PIPE, stdin=subprocess.PIPE, text=True)
# seed = 42
# level_name, landing_coords, states = play(proc, seed, commands)
# proc.kill()
#
# # initiate with seed
# line = proc.stdout.readline()
# if ("seed" not in line):
#     raise ValueError(f"expected prompt to enter seed. Was '{line}'")
# else:
#     proc.stdin.writelines([f"{seed}\n"])
#
# # initiate game
# level_name = proc.stdout.readline()
# # print(f"Level: {level_name}")
# num_landing_coords = int(proc.stdout.readline())
# landing_coords = []
# states = []
#
# # game loop
# poll = proc.poll()
# while poll is None and len(replay_commands) > 0:
#     # print("sending command")
#     command = replay_commands.popleft()
#     proc.stdin.writelines([f"{command}\n"])
#     # print(f'command "{command}" sent')
#
#     new_state = json.loads(proc.stdout.readline())
#     states.append(new_state)
#
#     if is_done(new_state):
#         break
#
#

# protocol:
# Enter seed:
# --> 42
# Level: 2-3
# 3 (num_landing_coords)
# 0 100
# 1000 500
# 6999 800

# loop:
# receive: json-dump of current state
# send: 0 0
