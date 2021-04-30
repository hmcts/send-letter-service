create table if not exists prints
(
    id uuid not null
        constraint message_pkey
            primary key,
    service varchar(256) not null,
    created_at timestamp not null,
    sent_to_print_at timestamp,
    printed_at timestamp,
    is_failed boolean default false,
    type varchar(256) not null,
    status varchar(256),
    checksum varchar(256) not null,
    documents json,
    case_id varchar(256),
    case_ref varchar(256),
    letter_type varchar(256)
);

alter table prints owner to send_letter;

create unique index if not exists print_checksum_status
    on letters (checksum, status)
    where ((status)::text = 'NEW'::text);

