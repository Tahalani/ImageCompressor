##
## EPITECH PROJECT, 2023
## wolfram
## File description:
## Makefile
##

SRC		=	src/Main.hs		\

NAME	=	imageCompressor

CFLAGS+= -Wall -Wextra

all:	$(NAME)

$(NAME):
	stack build
	find -name $(NAME) -type f -exec mv {} . \; -quit

clean:
	rm -f $(NAME)
	rm -rf .stack-work

fclean: clean

re:		fclean all

debug: CFLAGS += -g
debug: re

.PHONY: all clean fclean re debug
